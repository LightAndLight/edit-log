{-# language GADTs, KindSignatures #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
module Main where

import Control.Monad (join, when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.KeyboardEvent as KeyboardEvent
import JSDOM.Types (DOMRect, HTMLInputElement)
import qualified JSDOM.Element as Element
import qualified JSDOM.DOMRect as DOMRect
import qualified JSDOM.HTMLElement as HTMLElement
import qualified JSDOM.HTMLInputElement as HTMLInputElement
import qualified JSDOM.Types as JSDOM
import Language.Javascript.JSaddle.Monad (MonadJSM)
import Reflex
import Reflex.Dom (DomBuilder, DomBuilderSpace, GhcjsDomSpace, HasDocument, mainWidgetWithHead)
import qualified Reflex.Dom as Dom

import Hash (Hash)
import Log (Entry, Time)
import Node (Node(..))
import NodeType (KnownNodeType, NodeType(..), nodeType)
import Path (Path(..), Level(..))
import qualified Path as Path
import Syntax (Block(..), Statement(..), Expr(..), BinOp(..), UnOp(..), Ident(..))
import Session (Session, newSession)
import Session.Pure (runSessionT)
import qualified Store
import qualified Versioned
import Versioned.Pure (Versioned, runVersionedT, newVersioned)

import Focus (Focus(..))
import Navigation (nextHole, prevHole)

newtype Attrs = Attrs { unAttrs :: Map Text Text }

(=:) :: Text -> Text -> Attrs
(=:) k v = Attrs $ Map.singleton k v

infix 7 =:

instance Semigroup Attrs where
  Attrs as <> Attrs bs =
    Attrs $
    Map.unionWithKey
      (\k a b ->
         if k == "class"
         then
           if a == ""
           then b
           else
             if b == ""
             then a
             else a <> " " <> b
         else b
      )
      as
      bs

instance Monoid Attrs where; mempty = Attrs mempty

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

data NodeControls t
  = NodeControls
  { ncOpenMenu :: Event t ()
  , ncCloseMenu :: Event t ()
  }

data NodeEvent a where
  OpenMenu :: NodeEvent a
  CloseMenu :: NodeEvent a
  ContextMenuEvent :: ContextMenuEvent a -> NodeEvent a
  Select :: KnownNodeType b => Path a b -> NodeEvent a
deriving instance Show (NodeEvent a)

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

data ContextMenuEntry :: * -> * where
  EntryTrue :: ContextMenuEntry Expr
  EntryFalse :: ContextMenuEntry Expr
  EntryInt :: ContextMenuEntry Expr
  EntryAdd :: ContextMenuEntry Expr
  EntrySubtract :: ContextMenuEntry Expr
  EntryMultiply :: ContextMenuEntry Expr
  EntryDivide :: ContextMenuEntry Expr
  EntryOr :: ContextMenuEntry Expr
  EntryAnd :: ContextMenuEntry Expr
  EntryNot :: ContextMenuEntry Expr
  EntryNeg :: ContextMenuEntry Expr

  EntryFor :: ContextMenuEntry Statement
  EntryIfThen :: ContextMenuEntry Statement
  EntryIfThenElse :: ContextMenuEntry Statement
  EntryPrint :: ContextMenuEntry Statement
  EntryDef :: ContextMenuEntry Statement

  EntryIdent :: String -> ContextMenuEntry Ident
deriving instance Show (ContextMenuEntry a)

entryTitle :: ContextMenuEntry a -> Text
entryTitle entry =
  case entry of
    EntryTrue -> "true"
    EntryFalse -> "false"
    EntryInt -> "int"
    EntryAdd -> "add"
    EntrySubtract -> "subtract"
    EntryMultiply -> "multiply"
    EntryDivide -> "divide"
    EntryOr -> "or"
    EntryAnd -> "and"
    EntryNot -> "not"
    EntryNeg -> "negate"

    EntryFor -> "for"
    EntryIfThen -> "if then"
    EntryIfThenElse -> "if then else"
    EntryPrint -> "print"
    EntryDef -> "function definition"

    EntryIdent i -> Text.pack i

data ContextMenuEvent a where
  Choose :: KnownNodeType b => Path a b -> ContextMenuEntry b -> ContextMenuEvent a
  Next :: ContextMenuEvent a
  Prev :: ContextMenuEvent a
deriving instance Show (ContextMenuEvent a)

data ContextMenuSelection
  = TextInput
  | MenuEntry Int
  deriving Eq

menuNext :: Int -> ContextMenuSelection -> ContextMenuSelection
menuNext itemCount s =
  case s of
    TextInput -> MenuEntry 0
    MenuEntry n
      | n < itemCount -> MenuEntry $ n+1
      | otherwise -> TextInput

menuPrev :: Int -> ContextMenuSelection -> ContextMenuSelection
menuPrev itemCount s =
  case s of
    TextInput -> MenuEntry $ itemCount-1
    MenuEntry n ->
      if n == 0
      then TextInput
      else MenuEntry $ n-1

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
    dSelection :: Dynamic t ContextMenuSelection <-
      foldDyn
        ($)
        TextInput
        (mergeWith
           (.)
           [ menuNext count <$ cmcNext controls
           , menuPrev count <$ cmcPrev controls
           , const TextInput <$ ffilter id (updated dInputFocused)
           ])

    (dInputFocused, eContextMenu) <- renderInputField dSelection

    (count, eContextMenu') <-
      case nodeType @b of
        TBlock ->
          renderEntries dInputFocused dSelection []
        TExpr ->
          renderEntries dInputFocused dSelection
          [ EntryTrue
          , EntryFalse
          , EntryInt
          , EntryAdd
          , EntrySubtract
          , EntryMultiply
          , EntryDivide
          , EntryOr
          , EntryAnd
          , EntryNot
          , EntryNeg
          ]
        TStatement ->
          renderEntries dInputFocused dSelection
          [ EntryFor
          , EntryIfThen
          , EntryIfThenElse
          , EntryPrint
          , EntryDef
          ]
        TIdent -> do
          renderEntries dInputFocused dSelection []

  pure $ leftmost [eContextMenu, eContextMenu']
  where
    renderInputField ::
      Dynamic t ContextMenuSelection ->
      m (Dynamic t Bool, Event t (ContextMenuEvent a))
    renderInputField dSelected = do
      (inputElement, _) <- Dom.elAttr' "input" ("type" Dom.=: "text" <> "id" Dom.=: "context-menu-input") $ pure ()

      let
        htmlInputElement :: HTMLInputElement
        htmlInputElement =
          JSDOM.uncheckedCastTo JSDOM.HTMLInputElement (Dom._element_raw inputElement)

      eInput :: Event t Text <-
        fmap (fmapMaybe id) .
        Dom.wrapDomEvent htmlInputElement (`EventM.on` Events.input) $ do
          mTarget <- EventM.eventTarget
          case mTarget of
            Nothing -> pure Nothing
            Just target ->
              lift $ Just <$> HTMLInputElement.getValue (JSDOM.uncheckedCastTo JSDOM.HTMLInputElement target)

      dInputValue <- holdDyn "" eInput

      ePostBuild <- delay 0.05 =<< getPostBuild
      performEvent_ $
        HTMLElement.focus htmlInputElement <$ ePostBuild

      let
        eFocus = Dom.domEvent Dom.Focus inputElement
        eBlur = Dom.domEvent Dom.Blur inputElement

      dFocused :: Dynamic t Bool <- holdDyn True $ leftmost [False <$ eBlur, True <$ eFocus]

      performEvent_ $
        attachWithMaybe
          (\focused selected ->
            case selected of
              TextInput -> Just $ HTMLElement.focus htmlInputElement
              _
                | focused -> Just $ HTMLElement.blur htmlInputElement
                | otherwise -> Nothing
          )
          (current dFocused)
          (updated dSelected)

      let
        eChoose =
          case nodeType @b of
            TIdent ->
              attachWithMaybe
                (\(selected, value) () ->
                   case selected of
                     TextInput -> Just . Choose path $ EntryIdent (Text.unpack value)
                     _ -> Nothing
                )
                ((,) <$> current dSelected <*> current dInputValue)
                (cmcChoose controls)
            _ -> never

      pure (dFocused, eChoose)

    renderEntries ::
      Dynamic t Bool ->
      Dynamic t ContextMenuSelection ->
      [ContextMenuEntry b] ->
      m (Int, Event t (ContextMenuEvent a))
    renderEntries dInputFocused dSelection entries =
      Dom.elAttr "div" ("id" Dom.=: "context-menu-entries") $ do
        for_ (zip [0..] entries) $ \(ix, entry) ->
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
            Dom.text $ entryTitle entry
        pure
          ( length entries
          , attachWithMaybe
              (\selection () ->
                case selection of
                  MenuEntry ix -> Just . Choose path $ entries !! ix
                  _ -> Nothing
              )
              (current dSelection)
              (gate (not <$> current dInputFocused) (cmcChoose controls))
          )

syntaxHole :: DomBuilder t m => Attrs -> m ()
syntaxHole attrs =
  Dom.elAttr "div" (unAttrs attrs) $
  Dom.text "?"

renderIdent ::
  DomBuilder t m =>
  Ident ->
  m ()
renderIdent i =
  syntaxNode mempty $
  case i of
    Ident n -> Dom.text $ Text.pack n
    IHole -> syntaxHole mempty

syntaxInline :: DomBuilder t m => Attrs -> m a -> m a
syntaxInline attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-inline" <> attrs

syntaxLine :: DomBuilder t m => Attrs -> m a -> m a
syntaxLine attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-line" <> attrs

syntaxNode :: DomBuilder t m => Attrs -> m a -> m a
syntaxNode attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-node" <> attrs

syntaxNode' :: DomBuilder t m => Attrs -> m a -> m (Dom.Element Dom.EventResult (DomBuilderSpace m) t, a)
syntaxNode' attrs = Dom.elAttr' "div" . unAttrs $ "class" =: "syntax-node" <> attrs

syntaxNodeD' :: (DomBuilder t m, PostBuild t m) => Dynamic t Attrs -> m a -> m (Dom.Element Dom.EventResult (DomBuilderSpace m) t, a)
syntaxNodeD' attrs = Dom.elDynAttr' "div" . fmap unAttrs $ ("class" =: "syntax-node" <>) <$> attrs

syntaxKeyword :: DomBuilder t m => Attrs -> m a -> m a
syntaxKeyword attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-keyword" <> attrs

syntaxLiteral :: DomBuilder t m => Attrs -> m a -> m a
syntaxLiteral attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-literal" <> attrs

syntaxSymbol :: DomBuilder t m => Attrs -> m a -> m a
syntaxSymbol attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-symbol" <> attrs

syntaxLParen :: DomBuilder t m => Attrs -> m ()
syntaxLParen attrs = syntaxSymbol ("class" =: "syntax-paren" <> attrs) $ Dom.text "("

syntaxRParen :: DomBuilder t m => Attrs -> m ()
syntaxRParen attrs = syntaxSymbol ("class" =: "syntax-paren" <> attrs) $ Dom.text ")"

syntaxColon :: DomBuilder t m => Attrs -> m ()
syntaxColon attrs = syntaxSymbol ("class" =: "syntax-colon" <> attrs) $ Dom.text ":"

syntaxNested :: DomBuilder t m => Attrs -> m a -> m a
syntaxNested attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-nested" <> attrs

data NodeInfo t
  = NodeInfo
  { niHovered :: Dynamic t Bool
  }

instance Reflex t => Semigroup (NodeInfo t) where
  a <> b = NodeInfo { niHovered = (||) <$> niHovered a <*> niHovered b }

instance Reflex t => Monoid (NodeInfo t) where
  mempty = NodeInfo { niHovered = pure False }

isHole :: Node a -> Bool
isHole n =
  case n of
  NFor{} -> False
  NIfThen{} -> False
  NIfThenElse{} -> False
  NPrint{} -> False
  NDef{} -> False
  NBool{} -> False
  NInt{} -> False
  NBinOp{} -> False
  NUnOp{} -> False
  NBlock{} -> False
  NIdent{} -> False

  NSHole{} -> True
  NEHole{} -> True
  NIHole{} -> True

renderNode ::
  forall t m a b.
  ( MonadHold t m, DomBuilder t m, PostBuild t m
  , PerformEvent t m, MonadJSM (Performable m)
  , DomBuilderSpace m ~ GhcjsDomSpace
  , TriggerEvent t m
  , MonadFix m, MonadJSM m
  , KnownNodeType b
  ) =>
  KnownNodeType b =>
  NodeControls t ->
  ContextMenuControls t ->
  Dynamic t Menu ->
  Versioned a ->
  Focus b ->
  Path a b ->
  Bool ->
  Dynamic t Bool ->
  Maybe (Node b) ->
  m (Dom.Element Dom.EventResult GhcjsDomSpace t, (Event t (NodeEvent a), NodeInfo t))
renderNode controls contextMenuControls dMenu versioned focus path inFocus dHovered mNode =
  let
    dAttrs =
      (pure $
        if Maybe.maybe False isHole mNode
        then "class" =: "syntax-hole"
        else
          case nodeType @b of
            TExpr -> "class" =: "syntax-expr"
            TBlock -> "class" =: "syntax-block"
            TStatement -> "class" =: "syntax-statement"
            TIdent -> "class" =: "syntax-ident"
      ) <>
      fmap (\hovered -> if hovered then "class" =: "syntax-hovered" else mempty) dHovered <>
      if inFocus then pure ("class" =: "syntax-focused") else mempty
  in
    syntaxNodeD' dAttrs $
      case mNode of
        Nothing ->
          (never, mempty) <$ Dom.text "error: missing node"
        Just node ->
          case node of
            NIdent n ->
              (never, mempty) <$ Dom.text (Text.pack n)
            NIHole ->
              (never, mempty) <$ syntaxHole mempty

            NFor ident val body -> do
              ((eForIdent, forIdentInfo), (eForExpr, forExprInfo)) <-
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "for"
                  eForIdent <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      versioned
                      (case focus of
                        Focus (Cons For_Ident focusPath) -> Focus focusPath
                        _ -> NoFocus
                      )
                      (Path.snoc path For_Ident)
                      ident
                  syntaxKeyword mempty $ Dom.text "in"
                  eForExpr <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      versioned
                      (case focus of
                        Focus (Cons For_Expr focusPath) -> Focus focusPath
                        _ -> NoFocus
                      )
                      (Path.snoc path For_Expr)
                      val
                  syntaxColon mempty
                  pure (eForIdent, eForExpr)
              (eForBlock, forBlockInfo) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  versioned
                  (case focus of
                      Focus (Cons For_Block focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path For_Block)
                  body
              pure (leftmost [eForIdent, eForExpr, eForBlock], forIdentInfo <> forExprInfo <> forBlockInfo)
            NIfThen cond then_ -> do
              (eIfThenCond, ifThenCondInfo) <-
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "if"
                  ifThenCond <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      versioned
                      (case focus of
                        Focus (Cons IfThen_Cond focusPath) -> Focus focusPath
                        _ -> NoFocus
                      )
                      (Path.snoc path IfThen_Cond)
                      cond
                  syntaxColon mempty
                  pure ifThenCond
              (eIfThenThen, ifThenThenInfo) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  versioned
                  (case focus of
                      Focus (Cons IfThen_Then focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path IfThen_Then)
                  then_
              pure (leftmost [eIfThenCond, eIfThenThen], ifThenCondInfo <> ifThenThenInfo)
            NIfThenElse cond then_ else_ -> do
              (eIfThenElseCond, ifThenElseCondInfo) <-
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "if"
                  ifThenElseCond <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      versioned
                      (case focus of
                        Focus (Cons IfThenElse_Cond focusPath) -> Focus focusPath
                        _ -> NoFocus
                      )
                      (Path.snoc path IfThenElse_Cond)
                      cond
                  syntaxColon mempty
                  pure ifThenElseCond
              (eIfThenElseThen, ifThenElseThenInfo) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  versioned
                  (case focus of
                      Focus (Cons IfThenElse_Then focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path IfThenElse_Then)
                  then_
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "else"
                syntaxColon mempty
              (eIfThenElseElse, ifThenElseElseInfo) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  versioned
                  (case focus of
                      Focus (Cons IfThenElse_Else focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path IfThenElse_Else)
                  else_
              pure
                ( leftmost [eIfThenElseCond, eIfThenElseThen, eIfThenElseElse]
                , ifThenElseCondInfo <> ifThenElseThenInfo <> ifThenElseElseInfo
                )
            NPrint val ->
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "print"
                syntaxColon mempty
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  versioned
                  (case focus of
                      Focus (Cons Print_Value focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path Print_Value)
                  val
            NDef name args body -> do
              (eDefName, defNameInfo) <-
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "def"
                  defName <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      versioned
                      (case focus of
                          Focus (Cons Def_Name focusPath) -> Focus focusPath
                          _ -> NoFocus
                      )
                      (Path.snoc path Def_Name)
                      name
                  syntaxLParen mempty
                  case args of
                    [] -> pure ()
                    a : as -> do
                      renderIdent a
                      for_ as $ \x -> do
                        syntaxSymbol mempty $ Dom.text ","
                        renderIdent x
                  syntaxRParen mempty
                  syntaxColon mempty
                  pure defName
              (eDefBody, defBodyInfo) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  versioned
                  (case focus of
                      Focus (Cons Def_Body focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path Def_Body)
                  body
              pure (leftmost [eDefName, eDefBody], defNameInfo <> defBodyInfo)
            NBool b ->
              ((never, mempty) <$) . syntaxLiteral mempty . Dom.text $
              if b then "true" else "false"
            NInt n ->
              ((never, mempty) <$) . syntaxLiteral mempty . Dom.text $
              Text.pack (show n)
            NBinOp op left right ->
              syntaxInline mempty $ do
                (eBinOpLeft, binOpLeftInfo) <-
                  renderNodeHash
                    contextMenuControls
                    controls
                    dMenu
                    versioned
                    (case focus of
                        Focus (Cons BinOp_Left focusPath) -> Focus focusPath
                        _ -> NoFocus
                    )
                    (Path.snoc path BinOp_Left)
                    left
                case op of
                  Add -> syntaxSymbol mempty $ Dom.text "+"
                  Sub -> syntaxSymbol mempty $ Dom.text "-"
                  Mul -> syntaxSymbol mempty $ Dom.text "*"
                  Div -> syntaxSymbol mempty $ Dom.text "/"
                  And -> syntaxKeyword mempty $ Dom.text "and"
                  Or -> syntaxKeyword mempty $ Dom.text "or"
                (eBinOpRight, binOpRightInfo) <-
                  renderNodeHash
                    contextMenuControls
                    controls
                    dMenu
                    versioned
                    (case focus of
                        Focus (Cons BinOp_Right focusPath) -> Focus focusPath
                        _ -> NoFocus
                    )
                    (Path.snoc path BinOp_Right)
                    right
                pure (leftmost [eBinOpLeft, eBinOpRight], binOpLeftInfo <> binOpRightInfo)
            NUnOp op val ->
              syntaxInline mempty $ do
                case op of
                  Neg -> syntaxSymbol mempty $ Dom.text "-"
                  Not -> syntaxKeyword mempty $ Dom.text "not"
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  versioned
                  (case focus of
                      Focus (Cons UnOp_Value focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path UnOp_Value)
                  val
            NBlock sts -> do
              nodes <-
                traverse
                  (\(ix, st) ->
                    let
                      path' = Path.snoc path (Block_Index ix)
                    in
                      case focus of
                        Focus (Cons (Block_Index ix') focus') | ix == ix' ->
                          renderNodeHash contextMenuControls controls dMenu versioned (Focus focus') path' st
                        _ ->
                          renderNodeHash contextMenuControls controls dMenu versioned NoFocus path' st
                  )
                  (zip [0::Int ..] sts)
              pure (leftmost $ fst <$> nodes, foldMap snd nodes)
            NSHole ->
              (never, mempty) <$ syntaxHole mempty
            NEHole ->
              (never, mempty) <$ syntaxHole mempty

renderContextMenu ::
  forall t m a b.
  ( Reflex t, MonadHold t m
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m)
  , MonadJSM m, MonadFix m
  ) =>
  ContextMenuControls t ->
  Dynamic t Menu ->
  Focus b ->
  Path a b ->
  Dom.Element Dom.EventResult GhcjsDomSpace t ->
  m (Event t (ContextMenuEvent a))
renderContextMenu contextMenuControls dMenu focus path nodeElement =
  let
    mkMenu :: Menu -> m (Event t (ContextMenuEvent a))
    mkMenu menu =
      case menu of
        MenuOpen | Focus (Nil :: Path b c) <- focus -> do
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
          switchDyn <$> Dom.widgetHold (pure never) eRenderMenu
        _ ->
          pure never

    dMkMenu = mkMenu <$> dMenu
  in
    switchDyn <$> Dom.widgetHold (join . sample $ current dMkMenu) (updated dMkMenu)


renderNodeHash ::
  forall t m a b.
  ( MonadHold t m, DomBuilder t m, PostBuild t m
  , PerformEvent t m, MonadJSM (Performable m)
  , DomBuilderSpace m ~ GhcjsDomSpace
  , TriggerEvent t m
  , MonadFix m, MonadJSM m
  , KnownNodeType b
  ) =>
  ContextMenuControls t ->
  NodeControls t ->
  Dynamic t Menu ->
  Versioned a ->
  Focus b ->
  Path a b ->
  Hash b ->
  m (Event t (NodeEvent a), NodeInfo t)
renderNodeHash contextMenuControls controls dMenu versioned focus path h = do
  rec
    let
      eMouseenter = Dom.domEvent Dom.Mouseenter nodeElement
      eMouseleave = Dom.domEvent Dom.Mouseleave nodeElement
      eMousedown = Dom.domEvent Dom.Mousedown nodeElement

    let
      inFocus =
        case focus of
          Focus Nil -> True
          _ -> False

    dMouseInside <-
      case nodeType @b of
        TBlock -> pure $ niHovered childrenInfo
        _ -> holdDyn False $ leftmost [True <$ eMouseenter, False <$ eMouseleave]
    dHovered <-
      holdUniqDyn $
      case nodeType @b of
        TBlock -> pure False
        _ ->
          (\inside children -> inside && not children && not inFocus) <$>
          dMouseInside <*>
          niHovered childrenInfo

    let
      eClicked = gate (current dHovered) eMousedown

    let Identity (mNode, _) = runVersionedT versioned $ Store.lookupNode h
    (nodeElement, (eChildren, childrenInfo)) <-
      renderNode controls contextMenuControls dMenu versioned focus path inFocus dHovered mNode

  eContextMenu :: Event t (ContextMenuEvent a) <- renderContextMenu contextMenuControls dMenu focus path nodeElement

  let
    nodeInfo =
      NodeInfo
      { niHovered = dMouseInside
      }

  let
    eOpenMenu =
      attachWithMaybe
        (\menu () -> if inFocus && menu == MenuClosed then Just OpenMenu else Nothing)
        (current dMenu)
        (ncOpenMenu controls)

  pure
    ( leftmost
      [ eChildren
      , eOpenMenu
      , attachWithMaybe
          (\menu () -> if menu == MenuOpen then Just CloseMenu else Nothing)
          (current dMenu)
          (ncCloseMenu controls)
      , ContextMenuEvent <$> eContextMenu
      , Select path <$ eClicked
      ]
    , nodeInfo
    )

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

editor ::
  forall t m a.
  ( Reflex t, MonadHold t m, PostBuild t m, TriggerEvent t m
  , PerformEvent t m, MonadJSM (Performable m)
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , HasDocument m
  , MonadFix m, MonadJSM m
  , KnownNodeType a
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
      , cmcNext = dkDown keys
      , cmcPrev = dkUp keys
      }

    nodeControls =
      NodeControls
      { ncOpenMenu = dkSpace keys
      , ncCloseMenu = dkEscape keys
      }

    eNextHole = NextHole <$ dkTab keys
    ePrevHole = PrevHole <$ dkShiftTab keys

    initialVersioned :: Versioned a
    initialVersioned = newVersioned initial

    initialSession :: Session (Time, Entry a)
    initialSession = newSession

  rec
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
                   Choose path entry ->
                     Just . Replace path $
                     case entry of
                       EntryTrue -> Bool True
                       EntryFalse -> Bool False
                       EntryInt -> Int 0
                       EntryAdd -> BinOp Add EHole EHole
                       EntrySubtract -> BinOp Add EHole EHole
                       EntryMultiply -> BinOp Mul EHole EHole
                       EntryDivide -> BinOp Div EHole EHole
                       EntryOr -> BinOp Or EHole EHole
                       EntryAnd -> BinOp And EHole EHole
                       EntryNot -> UnOp Not EHole
                       EntryNeg -> UnOp Neg EHole
                       EntryFor -> For (Ident "x") EHole (Block [SHole])
                       EntryIfThen -> IfThen EHole (Block [SHole])
                       EntryIfThenElse -> IfThenElse EHole (Block [SHole]) (Block [SHole])
                       EntryPrint -> Print EHole
                       EntryDef -> Def IHole [Ident "x"] (Block [SHole])
                       EntryIdent i -> Ident i
                   _ -> Nothing
               Select path -> Just . SetFocus $ Focus path
               _ -> Nothing
             )
             eNode
         , eNextHole
         , ePrevHole
         ]
        )

    dMenu <-
      foldDyn
        (\nodeEvent now ->
          case nodeEvent of
            CloseMenu -> MenuClosed
            OpenMenu -> MenuOpen
            ContextMenuEvent Choose{} -> MenuClosed
            _ -> now
        )
        MenuClosed
        eNode

    let
      dRenderNodeHash :: Dynamic t (m (Event t (NodeEvent a)))
      dRenderNodeHash =
        (\versioned focus -> do
          let Identity (rooth, _) = runVersionedT versioned Versioned.getRoot
          (nodeEvent, _) <- renderNodeHash contextMenuControls nodeControls dMenu versioned focus Nil rooth
          pure nodeEvent
        ) <$>
        dVersioned <*>
        dFocus

    eNode :: Event t (NodeEvent a) <- do
      dNodeEvent :: Dynamic t (Event t (NodeEvent a)) <-
        Dom.widgetHold (join . sample $ current dRenderNodeHash) (updated dRenderNodeHash)
      pure $ switchDyn dNodeEvent

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
    )
    (editor
      (Block [SHole])
      (Focus $ Cons (Block_Index 0) Nil)
    )
