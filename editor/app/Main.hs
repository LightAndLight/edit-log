{-# language GADTs, KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
module Main where

import Control.Lens (view, _1, _2, _3)
import Control.Monad (join, when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.KeyboardEvent as KeyboardEvent
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
import Navigation (nextHole)

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
  }

documentKeys ::
  forall m t.
  ( Reflex t, TriggerEvent t m, HasDocument m, MonadJSM m
  , DomBuilderSpace m ~ GhcjsDomSpace
  ) =>
  m (DocumentKeys t)
documentKeys = do
  document <- Dom.askDocument
  let ctrl :: [Text] = ["Tab", "Enter"]
  eKeyDown :: Event t Text <-
    Dom.wrapDomEvent document (`EventM.on` Events.keyDown) $ do
      ev <- EventM.event
      code <- lift $ KeyboardEvent.getCode ev
      when (code `elem` ctrl) EventM.preventDefault
      pure code
  pure $
    DocumentKeys
    { dkSpace =
        fmapMaybe (\case; "Space" -> Just (); _ -> Nothing) eKeyDown
    , dkEscape =
        fmapMaybe (\case; "Escape" -> Just (); _ -> Nothing) eKeyDown
    , dkEnter =
        fmapMaybe (\case; "Enter" -> Just (); _ -> Nothing) eKeyDown
    , dkUp =
        fmapMaybe (\case; "ArrowUp" -> Just (); _ -> Nothing) eKeyDown
    , dkDown =
        fmapMaybe (\case; "ArrowDown" -> Just (); _ -> Nothing) eKeyDown
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

data ContextMenuEvent a where
  Choose :: KnownNodeType b => Path a b -> ContextMenuEntry b -> ContextMenuEvent a
  Next :: ContextMenuEvent a
  Prev :: ContextMenuEvent a
deriving instance Show (ContextMenuEvent a)

contextMenuEntries ::
  forall t m a b.
  ( MonadHold t m, DomBuilder t m, PostBuild t m, MonadFix m
  , KnownNodeType b
  ) =>
  ContextMenuControls t ->
  Path a b ->
  m (Event t (ContextMenuEvent a))
contextMenuEntries controls path = do
  rec
    dHighlighted :: Dynamic t Int <-
      holdDyn 0 $
      (\now f -> f now `mod` count) <$>
      current dHighlighted <@>
      mergeWith (.) [(\now -> now + 1) <$ cmcNext controls,  (\now -> now - 1) <$ cmcPrev controls]

    (count, eContextMenu) <-
      case nodeType @b of
        TBlock ->
          renderEntries dHighlighted []
        TExpr ->
          renderEntries dHighlighted
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
          renderEntries dHighlighted
          [ EntryFor
          , EntryIfThen
          , EntryIfThenElse
          , EntryPrint
          ]
  pure eContextMenu
  where
    renderEntries :: Dynamic t Int -> [ContextMenuEntry b] -> m (Int, Event t (ContextMenuEvent a))
    renderEntries dHighlighted entries = do
      for_ (zip [0..] entries) $ \(ix, entry) ->
        let
          dAttrs =
            (\active ->
              "class" Dom.=:
                ("context-menu-entry" <>
                 if ix == active then " context-menu-entry-highlighted" else ""
                )
            ) <$>
            dHighlighted
        in
          Dom.elDynAttr "div" dAttrs $
          Dom.text $ entryTitle entry
      pure
        ( length entries
        , Choose path . (entries !!) <$> current dHighlighted <@ cmcChoose controls
        )

renderIdent ::
  DomBuilder t m =>
  Ident ->
  m ()
renderIdent (Ident i) =
  syntaxNode mempty $ do
  Dom.text $ Text.pack i

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

syntaxSymbol :: DomBuilder t m => Attrs -> m a -> m a
syntaxSymbol attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-symbol" <> attrs

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

renderNode ::
  forall t m a b.
  ( MonadHold t m, DomBuilder t m, PostBuild t m, MonadFix m
  , KnownNodeType b
  ) =>
  ContextMenuControls t ->
  NodeControls t ->
  Menu ->
  Versioned a ->
  Focus b ->
  Path a b ->
  Hash b ->
  m (Event t (NodeEvent a), NodeInfo t)
renderNode contextMenuControls controls menu versioned focus path h = do
  rec
    let
      eMouseenter = Dom.domEvent Dom.Mouseenter nodeElement
      eMouseleave = Dom.domEvent Dom.Mouseleave nodeElement
      eMousedown = Dom.domEvent Dom.Mousedown nodeElement

    dMouseInside <- holdDyn False $ leftmost [True <$ eMouseenter, False <$ eMouseleave]
    dHovered <-
      holdUniqDyn $
      (\inside children -> inside && not children) <$>
      dMouseInside <*>
      niHovered childrenInfo

    let
      eClicked = gate (current dHovered) eMousedown

    let
      dAttrs =
        (pure $ case nodeType @b of
           TExpr -> "class" =: "syntax-expr"
           TBlock -> "class" =: "syntax-block"
           TStatement -> "class" =: "syntax-statement"
        ) <>
        fmap (\hovered -> if hovered then "class" =: "syntax-hovered" else mempty) dHovered <>
        if inFocus then pure ("class" =: "syntax-focused") else mempty

    (nodeElement, (eChildren, childrenInfo)) <-
      syntaxNodeD' dAttrs $ do
        let Identity (mNode, _) = runVersionedT versioned $ Store.lookupNode h
        case mNode of
          Nothing ->
            (never, mempty) <$ Dom.text "error: missing node"
          Just node ->
            case node of
              NFor ident val body -> do
                (eForExpr, forExprInfo) <-
                  syntaxLine mempty $ do
                    syntaxKeyword mempty $ Dom.text "for"
                    renderIdent ident
                    syntaxKeyword mempty $ Dom.text "in"
                    eForExpr <-
                      renderNode
                        contextMenuControls
                        controls
                        menu
                        versioned
                        (case focus of
                          Focus (Cons For_Expr focusPath) -> Focus focusPath
                          _ -> NoFocus
                        )
                        (Path.snoc path For_Expr)
                        val
                    syntaxSymbol mempty $ Dom.text ":"
                    pure eForExpr
                (eForBlock, forBlockInfo) <-
                  syntaxNested mempty $
                  renderNode
                    contextMenuControls
                    controls
                    menu
                    versioned
                    (case focus of
                        Focus (Cons For_Block focusPath) -> Focus focusPath
                        _ -> NoFocus
                    )
                    (Path.snoc path For_Block)
                    body
                pure (leftmost [eForExpr, eForBlock], forExprInfo <> forBlockInfo)
              NIfThen cond then_ -> do
                (eIfThenCond, ifThenCondInfo) <-
                  syntaxLine mempty $ do
                    syntaxKeyword mempty $ Dom.text "if"
                    ifThenCond <-
                      renderNode
                        contextMenuControls
                        controls
                        menu
                        versioned
                        (case focus of
                          Focus (Cons IfThen_Cond focusPath) -> Focus focusPath
                          _ -> NoFocus
                        )
                        (Path.snoc path IfThen_Cond)
                        cond
                    syntaxSymbol mempty $ Dom.text ":"
                    pure ifThenCond
                (eIfThenThen, ifThenThenInfo) <-
                  syntaxNested mempty $
                  renderNode
                    contextMenuControls
                    controls
                    menu
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
                      renderNode
                        contextMenuControls
                        controls
                        menu
                        versioned
                        (case focus of
                          Focus (Cons IfThenElse_Cond focusPath) -> Focus focusPath
                          _ -> NoFocus
                        )
                        (Path.snoc path IfThenElse_Cond)
                        cond
                    syntaxSymbol mempty $ Dom.text ":"
                    pure ifThenElseCond
                (eIfThenElseThen, ifThenElseThenInfo) <-
                  syntaxNested mempty $
                  renderNode
                    contextMenuControls
                    controls
                    menu
                    versioned
                    (case focus of
                        Focus (Cons IfThenElse_Then focusPath) -> Focus focusPath
                        _ -> NoFocus
                    )
                    (Path.snoc path IfThenElse_Then)
                    then_
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "else"
                  syntaxSymbol mempty $ Dom.text ":"
                (eIfThenElseElse, ifThenElseElseInfo) <-
                  syntaxNested mempty $
                  renderNode
                    contextMenuControls
                    controls
                    menu
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
                  syntaxSymbol mempty $ Dom.text ":"
                  renderNode
                    contextMenuControls
                    controls
                    menu
                    versioned
                    (case focus of
                        Focus (Cons Print_Value focusPath) -> Focus focusPath
                        _ -> NoFocus
                    )
                    (Path.snoc path Print_Value)
                    val
              NBool b ->
                ((never, mempty) <$) . syntaxKeyword mempty . Dom.text $
                if b then "true" else "false"
              NInt n ->
                ((never, mempty) <$) . syntaxKeyword mempty . Dom.text $
                Text.pack (show n)
              NBinOp op left right ->
                syntaxInline mempty $ do
                  (eBinOpLeft, binOpLeftInfo) <-
                    renderNode
                      contextMenuControls
                      controls
                      menu
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
                    renderNode
                      contextMenuControls
                      controls
                      menu
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
                  renderNode
                    contextMenuControls
                    controls
                    menu
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
                            renderNode contextMenuControls controls menu versioned (Focus focus') path' st
                          _ ->
                            renderNode contextMenuControls controls menu versioned NoFocus path' st
                    )
                    (zip [0::Int ..] sts)
                pure (leftmost $ fst <$> nodes, foldMap snd nodes)
              NSHole ->
                (never, mempty) <$ Dom.text "_"
              NEHole ->
                (never, mempty) <$ Dom.text "_"

  eContextMenu :: Event t (ContextMenuEvent a) <-
    case menu of
      MenuOpen | Focus (Nil :: Path b c) <- focus -> do
        Dom.elAttr "div" ("id" Dom.=: "context-menu") $ do
          let currentNodeType = nodeType @c
          Dom.elAttr "div" ("id" Dom.=: "context-menu-title") . Dom.el "i" . Dom.text $
            (case currentNodeType of
              TBlock -> "block"
              TExpr -> "expr"
              TStatement -> "statement"
            ) <> " menu"
          contextMenuEntries contextMenuControls path
      _ ->
        pure never

  let
    nodeInfo =
      NodeInfo
      { niHovered = dMouseInside
      }

  pure
    ( leftmost
      [ eChildren
      , fmapMaybe
          (\() -> if inFocus && menu == MenuClosed then Just OpenMenu else Nothing)
          (ncOpenMenu controls)
      , fmapMaybe
          (\() -> if menu == MenuOpen then Just CloseMenu else Nothing)
          (ncCloseMenu controls)
      , ContextMenuEvent <$> eContextMenu
      , Select path <$ eClicked
      ]
    , nodeInfo
    )
  where
    inFocus =
      case focus of
        Focus Nil -> True
        _ -> False

data EditAction a where
  Replace :: KnownNodeType b => Path a b -> b -> EditAction a

editor ::
  forall t m a.
  ( Reflex t, MonadHold t m, PostBuild t m, TriggerEvent t m
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

    initialVersioned :: Versioned a
    initialVersioned = newVersioned initial

    initialSession :: Session (Time, Entry a)
    initialSession = newSession

  rec
    (dVersioned, _dSession, dFocus) <-
      (\d -> (view _1 <$> d, view _2 <$> d, view _3 <$> d)) <$>
      foldDyn
        (\action (versioned, session, focus) ->
            let
              Identity (_, versioned', session') =
                runSessionT versioned session $
                case action of
                  Replace path val -> Versioned.replace path val
              focus' =
                case action of
                  Replace path _ ->
                    Maybe.fromMaybe focus $ nextHole versioned' path
            in
              (versioned', session', focus')
        )
        (initialVersioned, initialSession, initialFocus)
        (fmapMaybe
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
                 _ -> Nothing
             _ -> Nothing
          )
          eNode
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
      dRenderNode :: Dynamic t (m (Event t (NodeEvent a)))
      dRenderNode =
        (\menu versioned focus -> do
          let Identity (rooth, _) = runVersionedT versioned Versioned.getRoot
          (nodeEvent, _) <- renderNode contextMenuControls nodeControls menu versioned focus Nil rooth
          pure nodeEvent
        ) <$>
        dMenu <*>
        dVersioned <*>
        dFocus

    eNode :: Event t (NodeEvent a) <- do
      dNodeEvent :: Dynamic t (Event t (NodeEvent a)) <-
        Dom.widgetHold (join . sample $ current dRenderNode) (updated dRenderNode)
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
          "href" Dom.=: "https://fonts.googleapis.com/css2?family=Source+Code+Pro&display=swap"
         )
         (pure ())
       Dom.el "style" . Dom.text $
         Text.unlines
         [ "html { font-family: 'Source Code Pro', monospace; }"
         , ""
         , ".syntax-node {"
         , "  display: inline-block;"
         , "}"
         , ""
         , ".syntax-hovered {"
         , "  border: 1px solid lightgrey;"
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
         , ".syntax-node + .syntax-symbol {"
         , "  margin-left: 0.5em;"
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
         , ".syntax-keyword + .syntax-node {"
         , "  margin-left: 0.5em;"
         , "}"
         , ""
         , ".syntax-nested {"
         , "  margin-left: 1em;"
         , "}"
         , ""
         , ".syntax-focused {"
         , "  border: 1px solid black;"
         , "}"
         , ""
         , "#context-menu {"
         , "  border: 2px solid black;"
         , "}"
         , ""
         , ".context-menu-entry-highlighted {"
         , "  background-color: grey;"
         , "}"
         ]
    )
    (editor
      (Block [SHole])
      (Focus $ Cons (Block_Index 0) Nil)
    )
