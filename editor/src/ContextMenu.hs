{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables, TypeApplications #-}
module ContextMenu
  ( ContextMenuControls(..)
  , ContextMenuEvent(..)
  , Menu(..)
  , renderContextMenu
  )
where

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Functor (($>))
import qualified Data.List as List
import qualified Data.Ord
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHCJS.DOM.DOMRect as DOMRect
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLInputElement as HTMLInputElement
import GHCJS.DOM.Types (DOMRect, HTMLInputElement)
import qualified GHCJS.DOM.Types as GHCJS.DOM
import Language.Javascript.JSaddle.Monad (MonadJSM)
import Reflex
import Reflex.Dom (DomBuilder, DomBuilderSpace, GhcjsDomSpace)
import qualified Reflex.Dom as Dom

import NodeType (KnownNodeType, NodeType(..), nodeType)
import qualified Parser
import Path (Path)

import qualified Autocomplete
import Focus (Focus(..))

data Menu
  = MenuClosed
  | MenuOpen
  deriving Eq

data ContextMenuControls t
  = ContextMenuControls
  { cmcChoose :: Event t ()
  , cmcNext :: Event t ()
  , cmcPrev :: Event t ()
  }

data ContextMenuEvent a where
  Choose :: KnownNodeType b => Path a b -> b -> ContextMenuEvent a
  Next :: ContextMenuEvent a
  Prev :: ContextMenuEvent a

data MenuEntryIndex
  = MenuEntry Int
  deriving Eq

menuNext :: Int -> MenuEntryIndex -> MenuEntryIndex
menuNext itemCount s =
  case s of
    MenuEntry n
      | n < itemCount - 1 -> MenuEntry $ n+1
      | otherwise -> MenuEntry 0

menuPrev :: Int -> MenuEntryIndex -> MenuEntryIndex
menuPrev itemCount s =
  case s of
    MenuEntry n ->
      if n == 0
      then MenuEntry $ itemCount - 1
      else MenuEntry $ n-1

selectParser :: forall a. KnownNodeType a => Parser.Parser a
selectParser =
  case nodeType @a of
    TBlock -> error "no parser for block"
    TExpr -> Parser.expr
    TStatement -> Parser.simpleStatement
    TIdent -> Parser.ident
    TArgs -> Parser.args
    TParams -> Parser.params

contextMenuEntries ::
  forall t m a b.
  ( MonadHold t m, DomBuilder t m, PostBuild t m
  , PerformEvent t m, MonadJSM (Performable m)
  , TriggerEvent t m
  , MonadFix m, MonadJSM m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , KnownNodeType b
  ) =>
  ContextMenuControls t ->
  Path a b ->
  m (Event t (ContextMenuEvent a))
contextMenuEntries controls path = do
  rec
    dSelection :: Dynamic t MenuEntryIndex <-
      foldDyn
        ($)
        (MenuEntry 0)
        (mergeWith
           (.)
           [ menuNext <$> current dCount <@ cmcNext controls
           , menuPrev <$> current dCount <@ cmcPrev controls
           ])

    (_, dInputValue, eContextMenu) <- renderInputField

    let
      dParseResult :: Dynamic t (Text, Either Parser.ParseError b)
      dParseResult =
        (\inputValue ->
          ( inputValue
          , Parser.runParser (selectParser @b) $ Text.unpack inputValue
          )
        ) <$>
        dInputValue

      dEntryList :: Dynamic t [(Text, Either Parser.ParseError b)]
      dEntryList =
        (\(inputValue, parseResult) ->
          case nodeType @b of
            TBlock ->
              error "there are no suggestions for block"
            TExpr ->
              case parseResult of
                Left{} ->
                  fmap snd .
                  List.sortOn (Data.Ord.Down . fst) .
                  filter ((0 <) . fst) .
                  fmap (\(label, val) -> (Autocomplete.similarity inputValue label, (label, Right val))) $
                  Autocomplete.baseExprCompletions
                Right val ->
                  [(inputValue, Right val)]
            TStatement ->
              case parseResult of
                Left{} ->
                  fmap snd .
                  List.sortOn (Data.Ord.Down . fst) .
                  filter ((0 <) . fst) .
                  fmap (\(label, val) -> (Autocomplete.similarity inputValue label, (label, Right val))) $
                  Autocomplete.baseStatementCompletions
                Right val ->
                  [(inputValue, Right val)]
            TIdent ->
              case parseResult of
                Left{} ->
                  []
                Right val ->
                  [(inputValue, Right val)]
            TArgs ->
              case parseResult of
                Left{} ->
                  []
                Right val ->
                  [(inputValue, Right val)]
            TParams ->
              case parseResult of
                Left{} ->
                  []
                Right val ->
                  [(inputValue, Right val)]
        ) <$>
        dParseResult

    (dCount, eContextMenu') <- renderEntries dSelection dEntryList

  pure $ leftmost [eContextMenu, eContextMenu']
  where
    renderInputField ::
      m (Dynamic t Bool, Dynamic t Text, Event t (ContextMenuEvent a))
    renderInputField = do
      (inputElement, _) <-
        Dom.elAttr' "input"
          ("type" Dom.=: "text" <> "id" Dom.=: "context-menu-input" <> "autocomplete" Dom.=: "off")
          (pure ())

      let
        htmlInputElement :: HTMLInputElement
        htmlInputElement =
          GHCJS.DOM.uncheckedCastTo GHCJS.DOM.HTMLInputElement (Dom._element_raw inputElement)

      eInput :: Event t Text <-
        fmap (fmapMaybe id) .
        Dom.wrapDomEvent htmlInputElement (`EventM.on` Events.input) $ do
          mTarget <- EventM.eventTarget
          case mTarget of
            Nothing -> pure Nothing
            Just target ->
              lift $ Just <$> HTMLInputElement.getValue (GHCJS.DOM.uncheckedCastTo GHCJS.DOM.HTMLInputElement target)

      dValue <- holdDyn "" eInput

      ePostBuild <- delay 0.05 =<< getPostBuild
      performEvent_ $
        HTMLElement.focus htmlInputElement <$ ePostBuild

      let
        eFocus = Dom.domEvent Dom.Focus inputElement
        eBlur = Dom.domEvent Dom.Blur inputElement

      dFocused :: Dynamic t Bool <- holdDyn True $ leftmost [False <$ eBlur, True <$ eFocus]

      pure (dFocused, dValue, never)

    renderEntry ::
      Dynamic t MenuEntryIndex ->
      Int ->
      (Text, Either Parser.ParseError b) ->
      m ()
    renderEntry dSelection ix (entryTitle, _) =
      let
        dAttrs =
          (\selection ->
            "class" Dom.=:
              ("context-menu-entry" <>
              if MenuEntry ix == selection then " context-menu-entry-highlighted" else ""
              )
          ) <$>
          dSelection
      in
        Dom.elDynAttr "div" dAttrs $
        Dom.text entryTitle

    renderEntries ::
      Dynamic t MenuEntryIndex ->
      Dynamic t [(Text, Either Parser.ParseError b)] ->
      m (Dynamic t Int, Event t (ContextMenuEvent a))
    renderEntries dSelection dEntries =
      Dom.elAttr "div" ("id" Dom.=: "context-menu-entries") $ do
        Dom.dyn_ $ (\entries -> for_ (zip [0..] entries) (uncurry $ renderEntry dSelection)) <$> dEntries
        pure
          ( length <$> dEntries
          , attachWithMaybe
              (\(selection, entries) () ->
                case selection of
                  MenuEntry ix -> do
                    res <- lookup ix $ zip [0..] entries
                    case snd res of
                      Left{} -> Nothing
                      Right val -> Just $ Choose path val
              )
              ((,) <$> current dSelection <*> current dEntries)
              (cmcChoose controls)
          )

renderContextMenu ::
  forall t m a.
  ( Reflex t, MonadHold t m
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m)
  , MonadJSM m, MonadFix m
  ) =>
  ContextMenuControls t ->
  Dynamic t Menu ->
  Dynamic t (Focus a) ->
  Dynamic t (Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t)) ->
  m (Event t (ContextMenuEvent a))
renderContextMenu contextMenuControls dMenu dFocus dFocusElement = do
  dMkMenu <- do
    let dMkMenu_ = mkMenu <$> dMenu <*> dFocus <*> dFocusElement
    Dom.widgetHold (join . sample $ current dMkMenu_) (updated dMkMenu_)

  let
    eContextMenu = switchDyn dMkMenu

  pure eContextMenu
  where
    mkMenu ::
      Menu ->
      Focus a ->
      Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t) ->
      m (Event t (ContextMenuEvent a))
    mkMenu menu focus mNodeElement =
      case menu of
        MenuOpen | Focus path <- focus, Just nodeElement <- mNodeElement -> do
          -- we delay the rendering of the menu because it needs some JS properties that aren't
          -- present if we render immediately
          ePostBuild <- delay 0.05 =<< getPostBuild
          let
            eRenderMenu :: Event t (m (Event t (ContextMenuEvent a)))
            eRenderMenu =
              ePostBuild $> do
              (menuX, menuY) <- do
                nodeRect :: DOMRect <- Element.getBoundingClientRect $ Dom._element_raw nodeElement
                nodeX <- DOMRect.getX nodeRect
                nodeY <- DOMRect.getY nodeRect
                nodeHeight <- DOMRect.getHeight nodeRect
                pure (nodeX, nodeY + nodeHeight)
              let
                pos x y = Text.pack $ "left: " <> show x <> "px;" <> " " <> "top: " <> show y <> "px;"
                contextMenuAttrs = "id" Dom.=: "context-menu" <> "style" Dom.=: pos menuX menuY
              Dom.elAttr "div" contextMenuAttrs $ do
                contextMenuEntries contextMenuControls path

          dRenderMenu <- Dom.widgetHold (pure never) eRenderMenu

          let
            eContextMenu = switchDyn dRenderMenu

          pure eContextMenu
        _ ->
          pure never
