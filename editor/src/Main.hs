{-# language GADTs, KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
module Main where

import Control.Lens.Getter (view)
import Control.Monad (join, when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity(..))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.KeyboardEvent as KeyboardEvent
import Language.Javascript.JSaddle.Monad (MonadJSM)
import Reflex
import Reflex.Dom (DomBuilder, DomBuilderSpace, GhcjsDomSpace, HasDocument, mainWidgetWithHead)
import qualified Reflex.Dom as Dom

import Check (CheckError)
import qualified Check
import Log (Entry, Time)
import NodeType (KnownNodeType)
import Path (Path(..), Level(..))
import Path.Trie (Trie)
import qualified Path.Trie as Trie
import Syntax (Block(..), Statement(..))
import Session (Session, newSession)
import Session.Pure (runSessionT)
import qualified Versioned
import Versioned.Pure (Versioned, runVersionedT, newVersioned)

import ContextMenu (ContextMenuControls(..), ContextMenuEvent(..), Menu(..), renderContextMenu)
import Focus (Focus(..))
import Navigation (nextHole, prevHole)
import Render
  ( NodeControls(..), NodeEvent(..), RenderNodeEnv(..)
  , rniNodeEvent, rniFocusElement
  , renderNodeHash
  )

data DocumentKeys t
  = DocumentKeys
  { dkSpace :: Event t ()
  , dkEscape :: Event t ()
  , dkEnter :: Event t ()
  , dkUp :: Event t ()
  , dkDown :: Event t ()
  , dkTab :: Event t ()
  , dkShiftTab :: Event t ()
  }

data Keypress
  = Down Text
  | Up Text

documentKeys ::
  forall m t.
  ( Reflex t, MonadHold t m, TriggerEvent t m, HasDocument m
  , MonadJSM m, MonadFix m
  , DomBuilderSpace m ~ GhcjsDomSpace
  ) =>
  m (DocumentKeys t)
documentKeys = do
  document <- Dom.askDocument
  let ctrl :: [Text] = ["Tab", "Enter"]
  eKeyDown :: Event t Text <-
    Dom.wrapDomEvent document (`EventM.on` Events.keyDown) $ do
      ev <- EventM.event
      code <- lift $ KeyboardEvent.getKey ev
      when (code `elem` ctrl) EventM.preventDefault
      pure code
  eKeyUp :: Event t Text <-
    Dom.wrapDomEvent document (`EventM.on` Events.keyUp) $ do
      ev <- EventM.event
      code <- lift $ KeyboardEvent.getKey ev
      when (code `elem` ctrl) EventM.preventDefault
      pure code
  dHeld <-
    foldDyn
      (\keypress keys ->
         case keypress of
           Down key -> Set.insert key keys
           Up key -> Set.delete key keys
      )
      mempty
      (leftmost [Up <$> eKeyUp, Down <$> eKeyDown])
  pure $
    DocumentKeys
    { dkSpace =
        fmapMaybe (\case; " " -> Just (); _ -> Nothing) eKeyDown
    , dkEscape =
        fmapMaybe (\case; "Escape" -> Just (); _ -> Nothing) eKeyDown
    , dkEnter =
        fmapMaybe (\case; "Enter" -> Just (); _ -> Nothing) eKeyDown
    , dkUp =
        fmapMaybe (\case; "ArrowUp" -> Just (); _ -> Nothing) eKeyDown
    , dkDown =
        fmapMaybe (\case; "ArrowDown" -> Just (); _ -> Nothing) eKeyDown
    , dkTab =
        attachWithMaybe
          (\held pressed ->
             if pressed == "Tab" && Set.null held
             then Just ()
             else Nothing
          )
          (current dHeld)
          eKeyDown
    , dkShiftTab =
        attachWithMaybe
          (\held pressed ->
             if pressed == "Tab" && Set.member "Shift" held
             then Just ()
             else Nothing
          )
          (current dHeld)
          eKeyDown
    }

data EditAction a where
  Replace :: KnownNodeType b => Path a b -> b -> EditAction a
  NextHole :: EditAction a
  PrevHole :: EditAction a
  SetFocus :: Focus a -> EditAction a

data EditorState a
  = EditorState
  { esVersioned :: Versioned a
  , esSession :: Session (Time, Entry a)
  , esFocus :: Focus a
  }

data DocumentControls t
  = DocumentControls
  { dNextHole :: Event t ()
  , dPrevHole :: Event t ()
  }

editor ::
  forall t m a.
  ( Reflex t, MonadHold t m, PostBuild t m, TriggerEvent t m
  , PerformEvent t m, MonadJSM (Performable m)
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , HasDocument m
  , MonadFix m, MonadJSM m
  , KnownNodeType a
  , Show a
  ) =>
  a ->
  Focus a ->
  m ()
editor initial initialFocus = do
  keys <- documentKeys

  let
    contextMenuControls =
      ContextMenuControls
      { cmcChoose = dkEnter keys
      , cmcNext = leftmost [dkDown keys, dkTab keys]
      , cmcPrev = leftmost [dkUp keys, dkShiftTab keys]
      }

    nodeControls =
      NodeControls
      { ncOpenMenu = dkSpace keys
      , ncCloseMenu = dkEscape keys
      }

    documentControls =
      DocumentControls
      { dNextHole = dkTab keys
      , dPrevHole = dkShiftTab keys
      }

    initialVersioned :: Versioned a
    initialVersioned = newVersioned initial

    initialSession :: Session (Time, Entry a)
    initialSession = newSession


  rec
    let
      dMenuClosed = (MenuClosed ==) <$> current dMenu
      eNextHole = gate dMenuClosed (NextHole <$ dNextHole documentControls)
      ePrevHole = gate dMenuClosed (PrevHole <$ dPrevHole documentControls)

    (dVersioned, _dSession, dFocus) <-
      (\d -> (esVersioned <$> d, esSession <$> d, esFocus <$> d)) <$>
      foldDyn
        (\action editorState ->
            let
              Identity (_, versioned', session') =
                runSessionT (esVersioned editorState) (esSession editorState) $
                case action of
                  Replace path val -> do
                    _ <- Versioned.replace path val
                    pure ()
                  _ -> pure ()
              focus' =
                case action of
                  Replace path _ ->
                    Maybe.fromMaybe (esFocus editorState) $
                    nextHole versioned' path
                  NextHole ->
                    Maybe.fromMaybe (esFocus editorState) $
                    case esFocus editorState of
                      Focus path ->
                        nextHole versioned' path
                      NoFocus ->
                        nextHole versioned' Nil
                  PrevHole ->
                    Maybe.fromMaybe (esFocus editorState) $
                    case esFocus editorState of
                      Focus path ->
                        prevHole versioned' path
                      NoFocus ->
                        prevHole versioned' Nil
                  SetFocus newFocus -> newFocus
            in
              EditorState { esVersioned = versioned', esSession = session', esFocus = focus' }
        )
        (EditorState
         { esVersioned = initialVersioned
         , esSession = initialSession
         , esFocus = initialFocus
         }
        )
        (leftmost
         [ fmapMaybe
             (\case
               ContextMenuEvent event ->
                 case event of
                   Choose path a -> Just $ Replace path a
                   _ -> Nothing
               Select path -> Just . SetFocus $ Focus path
               _ -> Nothing
             )
             eNode
         , eNextHole
         , ePrevHole
         ]
        )

    dRootHash <-
      holdUniqDyn $
      (\versioned -> let Identity (rooth, _) = runVersionedT versioned Versioned.getRoot in rooth) <$>
      dVersioned

    let

      dCheckResult :: Dynamic t (Either (Trie a CheckError) ())
      dCheckResult =
        (\rootHash versioned ->
          let
            Identity (res, _) =
              runVersionedT versioned .
              Check.runCheckT Check.newCheckEnv Check.newCheckState $
              Check.check Nil rootHash
          in
            res
        ) <$>
        dRootHash <*>
        dVersioned

      dErrors :: Dynamic t (Trie a CheckError)
      dErrors = either id (const Trie.empty) <$> dCheckResult

    dMenu <-
      holdDyn
        MenuClosed
        (fmapMaybe
          (\case
            CloseMenu -> Just MenuClosed
            OpenMenu -> Just MenuOpen
            ContextMenuEvent Choose{} -> Just MenuClosed
            _ -> Nothing
          )
          eNode
        )

    let
      renderNodeEnv =
        RenderNodeEnv
        { _rnContextMenuControls = contextMenuControls
        , _rnNodeControls = nodeControls
        , _rnMenu = dMenu
        , _rnErrors = dErrors
        , _rnVersioned = dVersioned
        , _rnFocus = dFocus
        , _rnPath = Nil
        }

      dRenderNodeHash ::
        Dynamic t
          (m
             ( Event t (NodeEvent a)
             , Dynamic t (Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t))
             )
          )
      dRenderNodeHash =
        (\rootHash -> do
          ((), dRenderNodeInfo) <-
            runDynamicWriterT . flip runReaderT renderNodeEnv $
            renderNodeHash rootHash
          pure
            ( switchDyn $ view rniNodeEvent <$> dRenderNodeInfo
            , dRenderNodeInfo >>= view rniFocusElement
            )
        ) <$>
        dRootHash

    dRenderNodeHash' ::
      Dynamic t
        ( Event t (NodeEvent a)
        , Dynamic t (Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t))
        ) <-
      Dom.widgetHold (join . sample $ current dRenderNodeHash) (updated dRenderNodeHash)

    let
      dFocusElement :: Dynamic t (Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t))
      dFocusElement = dRenderNodeHash' >>= snd

      eRenderNode :: Event t (NodeEvent a)
      eRenderNode = switchDyn $ fst <$> dRenderNodeHash'

    let eNode = leftmost [ContextMenuEvent <$> eContextMenu, eRenderNode]

    eContextMenu :: Event t (ContextMenuEvent a) <-
      renderContextMenu contextMenuControls dMenu dFocus dFocusElement

  pure ()

main :: IO ()
main = do
  mainWidgetWithHead
    (do
       Dom.elAttr "meta" ("charset" Dom.=: "UTF-8") $ pure ()
       Dom.el "title" $ Dom.text "Editor"
       Dom.elAttr "link"
         ("rel" Dom.=: "stylesheet" <>
          "href" Dom.=: "https://fonts.googleapis.com/css2?family=Source+Code+Pro:ital,wght@0,400;0,500;1,400&display=swap"
         )
         (pure ())
       let
         bgColor = "#f8f8f8"

         keyword = "#354b98"
         literal = "#268884"
         symbol = "#974fbc"

         contextMenuBg = "#ececec"

         holeInactiveText = "rgba(0, 0, 0, 0.3)"
         holeInactive = "rgba(0, 0, 0, 0.2)"

         holeHoveredText = "rgba(0, 0, 0, 0.5)"
         holeHovered = "rgba(0, 0, 0, 0.4)"

         holeActiveText = "rgba(0, 0, 0, 0.6)"
         holeActive = "#fb3abe"

         nodeHoveredBg = "rgba(0, 0, 0, 0.025)"
         nodeHovered = "rgba(0, 0, 0, 0.4)"

         nodeActiveBg = "rgba(0, 0, 0, 0.05)"
         nodeActive = "#fb3abe"

         inputFocus = "rgb(255,131,208)"

         errorInactive = "#ff000060"
         errorHovered = "#ff0000A0"
         errorActive = "#ff0000"

       Dom.el "style" . Dom.text $
         Text.unlines
         [ "html {"
         , "  font-family: 'Source Code Pro', monospace;"
         , "  background-color: " <> bgColor <> ";"
         , "}"
         , ""
         , "input {"
         , "  font-family: 'Source Code Pro', monospace;"
         , "  font-size: 1em;"
         , "}"
         , ""
         , ".syntax-node {"
         , "  display: inline-block;"
         , "}"
         , ""
         , ".syntax-hole {"
         , "  box-shadow: inset 0 -1px 0 " <> holeInactive <> ";"
         , "  font-style: italic;"
         , "  color: " <> holeInactiveText <> ";"
         , "}"
         , ""
         , ".syntax-statement {"
         , "  padding-left: 0.25em;"
         , "  padding-right: 0.25em;"
         , "  border-radius: 0.1em;"
         , "}"
         , ""
         , ".syntax-focused.syntax-node {"
         , "  background-color: " <> nodeActiveBg <> ";"
         , "}"
         , ""
         , ".syntax-hovered.syntax-node {"
         , "  background-color: " <> nodeHoveredBg <> ";"
         , "}"
         , ""
         , ".syntax-focused.syntax-statement {"
         , "  box-shadow: inset 2px 0px 0 " <> nodeActive <> ";"
         , "}"
         , ""
         , ".syntax-hovered.syntax-statement {"
         , "  box-shadow: inset 2px 0px 0 " <> nodeHovered <> ";"
         , "}"
         , ""
         , ".syntax-focused.syntax-expr {"
         , "  box-shadow: inset 0 -2px 0 " <> holeActive <> ";"
         , "}"
         , ""
         , ".syntax-hovered.syntax-expr {"
         , "  box-shadow: inset 0 -1px 0 " <> holeHovered <> ";"
         , "}"
         , ""
         , ".syntax-focused.syntax-expr.has-error {"
         , "  box-shadow: none;"
         , "}"
         , ""
         , ".syntax-hovered.syntax-expr.has-error {"
         , "  box-shadow: none;"
         , "  text-decoration-color: " <> errorHovered <> ";"
         , "}"
         , ""
         , ".syntax-focused.syntax-args {"
         , "  box-shadow: inset 0 -2px 0 " <> holeActive <> ";"
         , "}"
         , ""
         , ".syntax-hovered.syntax-args {"
         , "  box-shadow: inset 0 -1px 0 " <> holeHovered <> ";"
         , "}"
         , ""
         , ".syntax-focused.syntax-params {"
         , "  box-shadow: inset 0 -2px 0 " <> holeActive <> ";"
         , "}"
         , ""
         , ".syntax-hovered.syntax-params {"
         , "  box-shadow: inset 0 -1px 0 " <> holeHovered <> ";"
         , "}"
         , ""
         , ".syntax-focused.syntax-ident {"
         , "  box-shadow: inset 0 -2px 0 " <> holeActive <> ";"
         , "}"
         , ""
         , ".syntax-hovered.syntax-ident {"
         , "  box-shadow: inset 0 -1px 0 " <> holeHovered <> ";"
         , "}"
         , ""
         , ".syntax-focused.syntax-hole {"
         , "  box-shadow: inset 0 -2px 0 " <> holeActive <> ";"
         , "  color: " <> holeActiveText <> ";"
         , "}"
         , ""
         , ".syntax-hovered.syntax-hole {"
         , "  box-shadow: inset 0 -1px 0 " <> holeHovered <> ";"
         , "  color: " <> holeHoveredText <> ";"
         , "}"
         , ""
         , ".syntax-focused {"
         , "}"
         , ""
         , ".syntax-literal {"
         , "  color: " <> literal <> ";"
         , "}"
         , ".syntax-keyword {"
         , "  color: " <> keyword <> ";"
         , "  font-weight: 500;"
         , "}"
         , ""
         , ".syntax-symbol {"
         , "  color: " <> symbol <> ";"
         , "}"
         , ""
         , ".syntax-hovered {"
         , "}"
         , ""
         , ".syntax-inline {"
         , "  display: flex;"
         , "  flex-direction: row;"
         , "  align-items: center;"
         , "}"
         , ""
         , ".syntax-line {"
         , "  display: flex;"
         , "  flex-direction: row;"
         , "  align-items: center;"
         , "  margin-top: 0.2em;"
         , "  margin-bottom: 0.2em;"
         , "}"
         , ""
         , ".syntax-block {"
         , "  width: 100%;"
         , "  box-sizing: border-box;"
         , "}"
         , ""
         , ".syntax-node + .syntax-symbol {"
         , "  margin-left: 0.5em;"
         , "}"
         , ""
         , ".syntax-node + .syntax-symbol.syntax-paren {"
         , "  margin-left: 0em;"
         , "}"
         , ""
         , ".syntax-node + .syntax-symbol.syntax-colon {"
         , "  margin-left: 0em;"
         , "}"
         , ""
         , ".syntax-node + .syntax-symbol.syntax-comma {"
         , "  margin-left: 0em;"
         , "}"
         , ""
         , ".syntax-node + .syntax-keyword {"
         , "  margin-left: 0.5em;"
         , "}"
         , ""
         , ".syntax-keyword + .syntax-keyword {"
         , "  margin-left: 0.5em;"
         , "}"
         , ""
         , ".syntax-symbol + .syntax-node {"
         , "  margin-left: 0.5em;"
         , "}"
         , ""
         , ".syntax-symbol.syntax-paren + .syntax-node {"
         , "  margin-left: 0em;"
         , "}"
         , ""
         , ".syntax-keyword + .syntax-node {"
         , "  margin-left: 0.5em;"
         , "}"
         , ""
         , ".syntax-nested {"
         , "  margin-left: 1em;"
         , "}"
         , ""
         , "#context-menu {"
         , "  background-color: " <> contextMenuBg <> ";"
         , "  border: 0px solid transparent;"
         , "  border-radius: 0.1em;"
         , "  position: absolute;"
         , "  box-shadow: 0 3px 4px 0px rgba(0, 0, 0, 0.4);"
         , "  padding: 0.25em;"
         , "}"
         , ""
         , "#context-menu-entries {"
         , "}"
         , ""
         , ".context-menu-entry {"
         , "  padding-left: 0.25em;"
         , "  padding-top: 0.125em;"
         , "  padding-bottom: 0.125em;"
         , "}"
         , ""
         , ".context-menu-entry-highlighted {"
         , "  background-color: " <> inputFocus <> ";"
         , "}"
         , ""
         , "#context-menu-input {"
         , "  outline: none;"
         , "  width: 100%;"
         , "  box-sizing: border-box;"
         , "  padding: 0.25em;"
         , "  border-radius: 0.1em;"
         , "  border: 1px solid " <> holeInactive <> ";"
         , "}"
         , ""
         , "#context-menu-input:focus {"
         , "  border: 1px solid " <> inputFocus <> ";"
         , "}"
         , ""
         , ".has-error {"
         , "  text-decoration-line: underline;"
         , "  text-decoration-style: wavy;"
         , "  text-decoration-color: " <> errorInactive <> ";"
         , "}"
         , ""
         , ".has-error.syntax-focused {"
         , "  text-decoration-color: " <> errorActive <> ";"
         , "}"
         ]
    )
    (editor
      (Block $ pure SHole)
      (Focus $ Cons (Block_Index 0) Nil)
    )
