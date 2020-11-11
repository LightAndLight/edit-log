{-# language GADTs, KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
module Main where

import Control.Lens.Getter ((^.), view)
import Control.Lens.TH (makeLenses)
import Control.Lens.Tuple (_1, _2, _3)
import Control.Monad (when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Char (isLetter)
import Data.Constraint.Extras (has)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Misc (Const2(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
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
-- import qualified Check
import Hash (Hash)
import Log (Entry, Time)
-- import Node (Node(..))
import NodeType
  ( KnownNodeType
  -- , NodeType(..)
  , nodeType
  )
import Path (Path(..), Level(..), SomePath(..))
import qualified Path
import Path.Trie (Trie)
import qualified Path.Trie as Trie
import Syntax
  ( Block(..)
  , Statement(..)
  , Expr(..)
  -- , Ident(..)
  )
import Session (Session, newSession)
import qualified Session
import Session.Pure (runSessionT)
import qualified Store
import qualified Versioned
import Versioned.Pure (Versioned, runVersionedT, newVersioned)

import BottomPanel (renderBottomPanel, bpNextError, bpPrevError, bpSetFocus)
import ContextMenu (ContextMenuControls(..), ContextMenuEvent(..), Menu(..), renderContextMenu)
import Focus (Focus(..))
import qualified Navigation
import Render (NodeControls(..), NodeEvent(..), RenderNodeEnv(..), ChildInfo(..), Located(..), renderNodeHash)
import Svg (svgEl, svgElAttr)

data DocumentKeys t
  = DocumentKeys
  { dkSpace :: Event t ()
  , dkEscape :: Event t ()
  , dkEnter :: Event t ()
  , dkDelete :: Event t ()
  , dkUp :: Event t ()
  , dkDown :: Event t ()
  , dkTab :: Event t ()
  , dkShiftTab :: Event t ()
  , dkCtrlZ :: Event t ()
  , dkCtrlShiftZ :: Event t ()
  , dkLetter :: EventSelector t (Const2 Char ())
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
  let ctrl :: [Text] = ["Tab", "Enter", " "]
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
    , dkDelete =
        fmapMaybe (\case; "Delete" -> Just (); _ -> Nothing) eKeyDown
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
    , dkCtrlZ =
        attachWithMaybe
          (\held pressed ->
             if pressed == "z" && Set.member "Control" held
             then Just ()
             else Nothing
          )
          (current dHeld)
          eKeyDown
    , dkCtrlShiftZ =
        attachWithMaybe
          (\held pressed ->
             if pressed == "Z" && Set.member "Control" held
             then Just ()
             else Nothing
          )
          (current dHeld)
          eKeyDown
    , dkLetter =
        fanMap $
        fmapMaybe
          (\code ->
            case Text.uncons code of
              Just (c, "") | isLetter c -> Just $ Map.singleton c ()
              _ -> Nothing
          )
          eKeyDown
    }

data EditAction a where
  Replace :: KnownNodeType b => Path a b -> b -> EditAction a
  InsertBefore :: EditAction a
  InsertAfter :: EditAction a
  NextHole :: EditAction a
  PrevHole :: EditAction a
  -- Delete :: FocusedNode a -> EditAction a
  SetFocus :: Focus a -> EditAction a
  Undo :: EditAction a
  Redo :: EditAction a
instance Show a => Show (EditAction a) where
  showsPrec d (Replace p (b :: b)) =
    showParen (d > 10) $
    showString "Replace " .
    showsPrec 11 p .
    showString " " .
    has @Show (nodeType @b) (showsPrec 11 b)
  showsPrec _ InsertBefore = showString "InsertBefore"
  showsPrec _ InsertAfter = showString "After"
  showsPrec _ NextHole = showString "NextHole"
  showsPrec _ PrevHole = showString "PrevHole"
  {-
  showsPrec d (Delete a) =
    showParen (d > 10) $
    showString "Delete " .
    showsPrec 11 a
-}
  showsPrec d (SetFocus f) =
    showParen (d > 10) $
    showString "SetFocus " .
    showsPrec 11 f
  showsPrec _ Undo = showString "Undo"
  showsPrec _ Redo = showString "Redo"

data EditorState a
  = EditorState
  { esVersioned :: Versioned a
  , esSession :: Session (Time, Entry a, SomePath a)
  , esFocus :: Focus a
  }

runEditAction :: KnownNodeType a => EditAction a -> EditorState a -> EditorState a
runEditAction action editorState =
  let
    Identity (focus', versioned', session') =
      runSessionT (esVersioned editorState) (esSession editorState) $
      let focus = esFocus editorState in
      case action of
        NextHole ->
          pure . Maybe.fromMaybe (esFocus editorState) $
          case esFocus editorState of
            Focus path ->
              Navigation.nextHole versioned' path
            NoFocus ->
              Navigation.nextHole versioned' Nil
        PrevHole ->
          pure . Maybe.fromMaybe (esFocus editorState) $
          case esFocus editorState of
            Focus path ->
              Navigation.prevHole versioned' path
            NoFocus ->
              Navigation.prevHole versioned' Nil
        SetFocus newFocus -> pure newFocus
        Replace path val -> do
          _ <- Versioned.replace path val
          pure . Maybe.fromMaybe (esFocus editorState) $ Navigation.nextHole versioned' path
        InsertBefore
          | Focus path <- esFocus editorState ->
              case Path.unsnoc path of
                Path.UnsnocMore prefix final ->
                  case final of
                    Block_Index ix -> do
                      _ <- Versioned.insert prefix (ix, SHole)
                      pure $ Focus (Path.snoc prefix (Block_Index ix))
                    _ ->
                      pure focus
                Path.UnsnocEmpty ->
                  pure focus
          | otherwise ->
              pure focus
        InsertAfter
          | Focus path <- esFocus editorState ->
              case Path.unsnoc path of
                Path.UnsnocMore prefix final ->
                  case final of
                    Block_Index ix -> do
                      _ <- Versioned.insert prefix (ix+1, SHole)
                      pure $ Focus (Path.snoc prefix (Block_Index $ ix+1))
                    _ -> pure focus
                Path.UnsnocEmpty ->
                  pure focus
          | otherwise ->
              pure focus
              {-
        Delete (FocusedNode path hash (node :: Node b)) ->
          case nodeType @b of
            TIdent ->
              case node of
                NIHole ->
                  case Path.unsnoc path of
                    Path.UnsnocMore prefix (Params_Index ix) -> do
                      _ <- Versioned.delete prefix ix
                      pure .
                        Maybe.fromMaybe (esFocus editorState) $
                        Navigation.findNextHole versioned' path hash
                    _ -> pure focus
                _ -> do
                  _ <- Versioned.replace path IHole
                  pure focus
            TStatement ->
              case node of
                NSHole ->
                  case Path.unsnoc path of
                    Path.UnsnocMore prefix (Block_Index ix) -> do
                      _ <- Versioned.delete prefix ix
                      pure .
                        Maybe.fromMaybe (esFocus editorState) $
                        Navigation.findNextHole versioned' path hash
                    _ -> pure focus
                _ -> do
                  _ <- Versioned.replace path SHole
                  pure focus
            TExpr ->
              case node of
                NEHole ->
                  case Path.unsnoc path of
                    Path.UnsnocMore prefix (Args_Index ix) -> do
                      _ <- Versioned.delete prefix ix
                      pure .
                        Maybe.fromMaybe (esFocus editorState) $
                        Navigation.findNextHole versioned' path hash
                    Path.UnsnocMore prefix (Exprs_Index ix) -> do
                      _ <- Versioned.delete prefix ix
                      pure .
                        Maybe.fromMaybe (esFocus editorState) $
                        Navigation.findNextHole versioned' path hash
                    _ -> pure focus
                _ -> do
                  _ <- Versioned.replace path EHole
                  pure focus
            _ ->
              pure focus
-}
        Undo -> do
          mRes <- Session.undo
          pure $
            case mRes of
              Nothing ->
                focus
              Just (_, _, SomePath p) ->
                Focus p
        Redo -> do
          mRes <- Session.redo
          pure $
            case mRes of
              Nothing ->
                focus
              Just (_, _, SomePath p) ->
                Focus p
  in
    EditorState { esVersioned = versioned', esSession = session', esFocus = focus' }

data EditorControls t a
  = EditorControls
  { ecNextHole :: Event t ()
  , ecPrevHole :: Event t ()
  , ecNewLineAbove :: Event t ()
  , ecNewLineBelow :: Event t ()
  , ecDelete :: Event t ()
  , ecUndo :: Event t ()
  , ecRedo :: Event t ()
  , ecNextError :: Event t ()
  , ecPrevError :: Event t ()
  , ecSetFocus :: Event t (Focus a)
  }

data Editor t a
  = Editor
  { _eFocus :: Dynamic t (Focus a)
  , _eErrors :: Dynamic t (Trie a CheckError)
  }
makeLenses ''Editor

renderEditor ::
  forall t m a.
  ( Reflex t, MonadHold t m, PostBuild t m, TriggerEvent t m
  , PerformEvent t m, MonadJSM (Performable m)
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , HasDocument m
  , MonadFix m, MonadJSM m
  , KnownNodeType a
  , Show a
  ) =>
  DocumentKeys t ->
  EditorControls t a ->
  a ->
  Focus a ->
  m (Editor t a)
renderEditor keys _editorControls initial initialFocus =
  Dom.elAttr "div" [("id", "editor")] $ do
    let
      contextMenuControls =
        ContextMenuControls
        { cmcChoose = dkEnter keys
        , cmcNext = leftmost [dkDown keys, dkTab keys]
        , cmcPrev = leftmost [dkUp keys, dkShiftTab keys]
        }

      initialVersioned :: Versioned a
      initialVersioned = newVersioned initial

      _initialSession :: Session (Time, Entry a, SomePath a)
      _initialSession = newSession

      initialRootHash =
        let
          Identity (rooth, _) = runVersionedT initialVersioned Versioned.getRoot
        in
          rooth

    rec
      let
        nodeControls =
          NodeControls
          { _ncOpenMenu = gate ((== MenuClosed) <$> bMenu) (dkSpace keys)
          , _ncCloseMenu = dkEscape keys
          }

      let
        dErrors :: Dynamic t (Trie a CheckError)
        dErrors = constDyn Trie.empty
          -- mkErrors <$> dCheckResult

      let
        initialMenu = MenuClosed
        eMenu =
          fmapMaybe
            (\case
              CloseMenu -> Just MenuClosed
              OpenMenu -> Just MenuOpen
              _ -> Nothing
            )
            eNode
      bMenu <- hold initialMenu eMenu

      let
        eStep :: Event t (Maybe (Located a Hash), Maybe (Focus a), Maybe (Versioned a))
        eStep =
          attachWithMaybe
            (\old event ->
               case event of
                 Left contextMenuEvent ->
                   case contextMenuEvent of
                     Choose path val ->
                       let
                         Identity (hash, new) =
                           runVersionedT old $ do
                             h <- Store.addKnownNode val
                             _ <- Versioned.replaceH path h
                             pure h

                         mFocus = Navigation.nextHole new path
                       in
                         Just (Just (Located path hash), mFocus, Just new)
                     _ -> Nothing
                 Right nodeEvent ->
                   case nodeEvent of
                     Select p ->
                       Just (Nothing, Just (Focus p), Nothing)
                     _ -> Nothing
            )
            bVersioned
            (leftmost [Left <$> eContextMenu, Right <$> eNode])

        (eHashChanged, eFocusChanged, eVersionUpdated) =
          (fmapMaybe (view _1) eStep, fmapMaybe (view _2) eStep, fmapMaybe (view _3) eStep)

      bVersioned <- hold initialVersioned eVersionUpdated
      dFocus <- holdDyn initialFocus eFocusChanged

      let
        renderNodeEnv =
          RenderNodeEnv
          { _rnVersioned = bVersioned
          , _rnVersionedChanged = eVersionUpdated
          , _rnPath = Nil
          , _rnFocus = dFocus
          , _rnHashChanged = eHashChanged
          , _rnNodeControls = nodeControls
          }

      (((), eNode), dChildInfo) <-
        runDynamicWriterT . runEventWriterT $
        runReaderT (renderNodeHash initialVersioned initialRootHash) renderNodeEnv

      let dFocusElement = _ciFocusElement <$> dChildInfo
      eContextMenu :: Event t (ContextMenuEvent a) <-
        renderContextMenu
          contextMenuControls
          (initialMenu, initialFocus)
          (eMenu, eFocusChanged)
          (bMenu, current dFocus, dFocusElement)

    pure $ Editor { _eFocus = dFocus, _eErrors = dErrors }

stylesheet :: Text
stylesheet =
  let
    bgColor = "#f4f4f8"
    bottomPanelBg = "#e4e4e8"
    bottomPanelBorder = "#ccccd0"
    contextMenuBg = bottomPanelBg

    keyword = "#354b98"
    literal = "#268884"
    symbol = "#974fbc"

    holeInactiveText = "rgba(0, 0, 0, 0.3)"
    holeInactive = "rgba(0, 0, 0, 0.2)"

    holeHoveredText = "rgba(0, 0, 0, 0.5)"
    holeHovered = "rgba(0, 0, 0, 0.4)"

    holeActiveText = "rgba(0, 0, 0, 0.6)"
    holeActive = "#fb3abe"

    nodeHoveredBg = "rgba(0, 0, 0, 0.045)"
    nodeHovered = "rgba(0, 0, 0, 0.4)"

    nodeActiveBg = "rgba(0, 0, 0, 0.08)"
    nodeActive = "#fb3abe"

    inputFocus = "rgb(255,131,208)"

    -- errorInactive = "#ff000060"
    -- errorHovered = "#ff0000A0"
    -- errorActive = "#ff0000"
  in
    Text.unlines
    [ "html {"
    , "  font-family: 'Source Sans Pro', sans-serif;"
    , "  background-color: " <> bgColor <> ";"
    , "}"
    , ""
    , "body {"
    , "  height: 100vh;"
    , "}"
    , ""
    , "#content {"
    , "  display: flex;"
    , "  flex-direction: column;"
    , "  height: 100%;"
    , "}"
    , ""
    , "#editor {"
    , "  font-family: 'Source Code Pro', monospace;"
    , "  flex: 1;"
    , "  padding: 1em;"
    , "  height: 75vh;"
    , "  overflow: scroll;"
    , "  overflow-x: auto;"
    , "  overflow-y: auto;"
    , "}"
    , ""
    , "#bottom-panel {"
    , "  border-top: 1px solid " <> bottomPanelBorder <> ";"
    , "  background-color: " <> bottomPanelBg <> ";"
    , "  height: 25vh;"
    , "  color: #2f2f2f;"
    , "}"
    , ""
    , "#bottom-panel-header {"
    , "  padding-left: 1em;"
    , "  padding-right: 1em;"
    , "}"
    , ""
    , ".bottom-panel-header-item {"
    , "  display: inline-block;"
    , "  padding-top: 0.5em;"
    , "  padding-bottom: 0.75em;"
    , "}"
    , ""
    , ".bottom-panel-header-item-active {"
    , "  padding-bottom: 0.25em;"
    , "  box-shadow: inset 0 -2px 0 " <> holeActive <> ";"
    , "}"
    , ""
    , "#bottom-panel-body {"
    , "  padding-top: 0.5em;"
    , "  padding-bottom: 0.5em;"
    , "}"
    , ""
    , ".bottom-panel-error {"
    , "  padding-left: 1em;"
    , "  padding-right: 1em;"
    , "  padding-top: 0.25em;"
    , "  padding-bottom: 0.25em;"
    , "}"
    , ""
    , ".bottom-panel-error-hovered {"
    , "  background-color: " <> nodeHoveredBg <> ";"
    , "}"
    , ""
    , ".bottom-panel-error-focused {"
    , "  background-color: " <> nodeActiveBg <> ";"
    , "}"
    , ""
    , "body {"
    , "  width: 100%;"
    , "  margin: 0;"
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
    , "  margin-top: 0.25em;"
    , "  border-radius: 0.1em;"
    , "}"
    , ""
    , ".syntax-statement.syntax-hole {"
    , "  padding-left: 0em;"
    , "  padding-right: 0em;"
    , "  border-radius: 0em;"
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
    , ".syntax-statement.syntax-focused {"
    , "  box-shadow: inset 2px 0px 0 " <> nodeActive <> ";"
    , "}"
    , ""
    , ".syntax-statement.syntax-hovered {"
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
    , ".syntax-statement.syntax-focused.syntax-hole {"
    , "  box-shadow: inset 0 -2px 0 " <> holeActive <> ";"
    , "  color: " <> holeActiveText <> ";"
    , "}"
    , ""
    , ".syntax-statement.syntax-hovered.syntax-hole {"
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
    , "  display: flex;"
    , "  flex-direction: column;"
    , "  align-items: flex-start;"
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
    ]

main :: IO ()
main = do
  mainWidgetWithHead
    (do
       Dom.elAttr "meta" ("charset" Dom.=: "UTF-8") $ pure ()
       Dom.el "title" $ Dom.text "Editor"
       Dom.elAttr "link"
         ("rel" Dom.=: "stylesheet" <>
          "href" Dom.=: "https://fonts.googleapis.com/css2?family=family=Source+Code+Pro:ital,wght@0,400;0,500;1,400&display=swap"
         )
         (pure ())
       Dom.elAttr "link"
         ("rel" Dom.=: "stylesheet" <>
          "href" Dom.=: "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400&display=swap"
         )
         (pure ())
       Dom.elAttr "link"
         ("rel" Dom.=: "stylesheet" <>
          "href" Dom.=: "https://cdn.jsdelivr.net/npm/remixicon@2.5.0/fonts/remixicon.css"
         )
         (pure ())

       Dom.el "style" $ Dom.text stylesheet
    )
    (do
       svgElAttr "svg" [("style", "position: absolute; width: 10px; height: 10px;")] $ do
         svgEl "defs" $ do
           svgElAttr "pattern" [("id", "zig"), ("width", "5"), ("height", "2.5"), ("patternUnits", "userSpaceOnUse")]$ do
             svgElAttr "line" [("x1", "0"), ("y1", "0"), ("x2", "2.5"), ("y2", "2.5"), ("stroke-width", "1"), ("stroke", "#ff0000"), ("fill", "none")] $ pure ()
             svgElAttr "line" [("x1", "2.5"), ("y1", "2.5"), ("x2", "5"), ("y2", "0"), ("stroke-width", "1"), ("stroke", "#ff0000"), ("fill", "none")] $ pure ()
       keys <- documentKeys
       Dom.elAttr "div" [("id", "content")] $ do
         rec
           let
             editorControls =
               EditorControls
               { ecNextHole = dkTab keys
               , ecPrevHole = dkShiftTab keys
               , ecNewLineAbove = select (dkLetter keys) (Const2 'O')
               , ecNewLineBelow = select (dkLetter keys) (Const2 'o')
               , ecDelete = dkDelete keys
               , ecUndo = dkCtrlZ keys
               , ecRedo = dkCtrlShiftZ keys
               , ecNextError = bottomPanel ^. bpNextError
               , ecPrevError = bottomPanel ^. bpPrevError
               , ecSetFocus = bottomPanel ^. bpSetFocus
               }
             initialProgram =
               Block . NonEmpty.fromList $ replicate 100 (IfThenElse EHole (Block [SHole]) (Block [SHole]))
             initialFocus =
               Focus $ Cons (Block_Index 0) Nil
           editor <-
             renderEditor
               keys
               editorControls
               initialProgram
               initialFocus
           bottomPanel <- renderBottomPanel (editor ^. eFocus) (editor ^. eErrors)
         pure ()
    )
