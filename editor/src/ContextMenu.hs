{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# options_ghc -fno-warn-overlapping-patterns #-}
module ContextMenu
  ( ContextMenuControls(..)
  , ContextMenuEvent(..)
  , Menu(..)
  , renderContextMenu
  )
where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (lift)
import Data.Constraint.Extras (has)
import qualified Data.Dependent.Map as DMap
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Functor.Identity (runIdentity)
import Data.GADT.Compare (GEq(..), GCompare(..), GOrdering(..), (:~:)(..))
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
  deriving (Eq, Show)

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
instance Show a => Show (ContextMenuEvent a) where
  showsPrec d (Choose p (b :: b)) =
    showParen (d > 10) $
    showString "Choose " .
    showsPrec 11 p .
    showString " " .
    has @Show (nodeType @b) (showsPrec 11 b)
  showsPrec _ Next = showString "Next"
  showsPrec _ Prev = showString "Prev"

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
    TExprs -> Parser.exprs

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
            TExprs ->
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

data Changes t a x where
  MenuChanged :: Changes t a Menu
  FocusChanged :: Changes t a (Focus a)
  FocusElementChanged :: Changes t a (Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t))
instance GEq (Changes t a) where
  geq MenuChanged MenuChanged = Just Refl
  geq MenuChanged _ = Nothing
  geq _ MenuChanged = Nothing

  geq FocusChanged FocusChanged = Just Refl
  geq FocusChanged _ = Nothing
  geq _ FocusChanged = Nothing

  geq FocusElementChanged FocusElementChanged = Just Refl
  geq FocusElementChanged _ = Nothing
  geq _ FocusElementChanged = Nothing
instance GCompare (Changes t a) where
  gcompare MenuChanged MenuChanged = GEQ
  gcompare MenuChanged _ = GLT
  gcompare _ MenuChanged = GGT

  gcompare FocusChanged FocusChanged = GEQ
  gcompare FocusChanged _ = GLT
  gcompare _ FocusChanged = GGT

  gcompare FocusElementChanged FocusElementChanged = GEQ
  gcompare FocusElementChanged _ = GLT
  gcompare _ FocusElementChanged = GGT

renderContextMenu ::
  forall t m a.
  ( Reflex t, MonadHold t m
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m)
  , MonadJSM m, MonadFix m
  ) =>
  ContextMenuControls t ->
  (Menu, Focus a) ->
  (Event t Menu, Event t (Focus a)) ->
  Dynamic t (Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t)) ->
  m (Event t (ContextMenuEvent a))
renderContextMenu contextMenuControls (initialMenu, initialFocus) (eMenu, eFocus) dFocusElement = do
  bMenu <- hold initialMenu eMenu
  bFocus <- hold initialFocus eFocus

  let
    bFocusElement = current dFocusElement
    eFocusElement = updated dFocusElement
  initialFocusElement <- sample bFocusElement

  dMkMenu <- do
    Dom.widgetHold
      (mkMenu initialMenu initialFocus initialFocusElement)
      ((\menu focus focusElement changes ->
          let
            menu' = maybe menu runIdentity (DMap.lookup MenuChanged changes)
            focus' = maybe focus runIdentity (DMap.lookup FocusChanged changes)
            focusElement' = maybe focusElement runIdentity (DMap.lookup FocusElementChanged changes)
          in
            mkMenu menu' focus' focusElement'
       ) <$>
       bMenu <*>
       bFocus <*>
       bFocusElement <@>
       mergeWith (<>)
         [ DMap.singleton MenuChanged . pure <$> eMenu
         , DMap.singleton FocusChanged . pure <$> eFocus
         , DMap.singleton FocusElementChanged . pure <$> eFocusElement
         ]
      )

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
