{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables, TypeApplications #-}
module Render
  ( NodeControls(..), NodeEvent(..), NodeInfo(..)
  , renderNodeHash
  , syntaxNode
  , syntaxNode'
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Fix (MonadFix)
import Data.Foldable (asum)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Language.Javascript.JSaddle.Monad (MonadJSM)
import Reflex
import Reflex.Dom (DomBuilder, DomBuilderSpace, GhcjsDomSpace)
import qualified Reflex.Dom as Dom

import Check (CheckError)
import Hash (Hash)
import Node (Node(..))
import NodeType (KnownNodeType, NodeType(..), nodeType)
import Path (Path(..), Level(..))
import qualified Path
import Path.Trie (Trie)
import qualified Path.Trie as Trie
import Syntax (BinOp(..), UnOp(..))
import qualified Store
import Versioned.Pure (Versioned, runVersionedT)

import Attrs
import ContextMenu (ContextMenuControls(..), ContextMenuEvent(..), Menu(..))
import Focus (Focus(..))

syntaxHole :: DomBuilder t m => Attrs -> m ()
syntaxHole attrs =
  Dom.elAttr "div" (unAttrs attrs) $
  Dom.text "?"

syntaxInline :: DomBuilder t m => Attrs -> m a -> m a
syntaxInline attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-inline" <> attrs

syntaxLine :: DomBuilder t m => Attrs -> m a -> m a
syntaxLine attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-line" <> attrs

syntaxNode :: DomBuilder t m => Attrs -> m a -> m a
syntaxNode attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-node" <> attrs

syntaxNode' :: DomBuilder t m => Attrs -> m a -> m (Dom.Element Dom.EventResult (DomBuilderSpace m) t, a)
syntaxNode' attrs = Dom.elAttr' "div" . unAttrs $ "class" =: "syntax-node" <> attrs

syntaxNodeD' ::
  (DomBuilder t m, PostBuild t m) =>
  Dynamic t Attrs ->
  m a ->
  m (Dom.Element Dom.EventResult (DomBuilderSpace m) t, a)
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

syntaxComma :: DomBuilder t m => Attrs -> m ()
syntaxComma attrs = syntaxSymbol ("class" =: "syntax-comma" <> attrs) $ Dom.text ","

syntaxNested :: DomBuilder t m => Attrs -> m a -> m a
syntaxNested attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-nested" <> attrs

isHole :: Node a -> Bool
isHole n =
  case n of
    NFor{} -> False
    NIfThen{} -> False
    NIfThenElse{} -> False
    NPrint{} -> False
    NReturn{} -> False
    NDef{} -> False
    NBool{} -> False
    NInt{} -> False
    NEIdent{} -> False
    NBinOp{} -> False
    NUnOp{} -> False
    NCall{} -> False
    NBlock{} -> False
    NIdent{} -> False
    NArgs{} -> False
    NParams{} -> False

    NSHole{} -> True
    NEHole{} -> True
    NIHole{} -> True

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

data NodeInfo t
  = NodeInfo
  { niHovered :: Dynamic t Bool
  }

instance Reflex t => Semigroup (NodeInfo t) where
  a <> b = NodeInfo { niHovered = (||) <$> niHovered a <*> niHovered b }

instance Reflex t => Monoid (NodeInfo t) where
  mempty = NodeInfo { niHovered = pure False }

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
  Dynamic t (Trie b CheckError) ->
  Versioned a ->
  Focus b ->
  Path a b ->
  Hash b ->
  m
    ( Event t (NodeEvent a)
    , NodeInfo t
    , Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t)
    )
renderNodeHash contextMenuControls controls dMenu dErrors versioned focus path h = do
  rec
    let
      dError :: Dynamic t (Maybe (CheckError b))
      dError = Trie.current <$> dErrors

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
    (nodeElement, (eChildren, childrenInfo, childFocus)) <-
      renderNode controls contextMenuControls dMenu dErrors versioned focus path inFocus dError dHovered mNode

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
      , Select path <$ eClicked
      ]
    , nodeInfo
    , childFocus <|> (if inFocus then Just nodeElement else Nothing)
    )

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
  Dynamic t (Trie b CheckError) ->
  Versioned a ->
  Focus b ->
  Path a b ->
  Bool ->
  Dynamic t (Maybe (CheckError b)) ->
  Dynamic t Bool ->
  Maybe (Node b) ->
  m
    ( Dom.Element Dom.EventResult GhcjsDomSpace t
    , (Event t (NodeEvent a), NodeInfo t, Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t))
    )
renderNode controls contextMenuControls dMenu dErrors versioned focus path inFocus dError dHovered mNode =
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
            TArgs -> "class" =: "syntax-args"
            TParams -> "class" =: "syntax-params"
      ) <>
      fmap (\hovered -> if hovered then "class" =: "syntax-hovered" else mempty) dHovered <>
      fmap (\mError -> case mError of; Nothing -> mempty; Just _ -> "class" =: "has-error") dError <>
      if inFocus then pure ("class" =: "syntax-focused") else mempty
  in
    syntaxNodeD' dAttrs $
      case mNode of
        Nothing ->
          (never, mempty, Nothing) <$ Dom.text "error: missing node"
        Just node ->
          case node of
            NEIdent n ->
              (never, mempty, Nothing) <$ Dom.text (Text.pack n)
            NIdent n ->
              (never, mempty, Nothing) <$ Dom.text (Text.pack n)
            NIHole ->
              (never, mempty, Nothing) <$ syntaxHole mempty
            NFor ident val body -> do
              ((eForIdent, forIdentInfo, forIdentFocus), (eForExpr, forExprInfo, forExprFocus)) <-
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "for"
                  forIdent <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      (Maybe.fromMaybe Trie.empty . Trie.down For_Ident <$> dErrors)
                      versioned
                      (case focus of
                        Focus (Cons For_Ident focusPath) -> Focus focusPath
                        _ -> NoFocus
                      )
                      (Path.snoc path For_Ident)
                      ident
                  syntaxKeyword mempty $ Dom.text "in"
                  forExpr <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      (Maybe.fromMaybe Trie.empty . Trie.down For_Expr <$> dErrors)
                      versioned
                      (case focus of
                        Focus (Cons For_Expr focusPath) -> Focus focusPath
                        _ -> NoFocus
                      )
                      (Path.snoc path For_Expr)
                      val
                  syntaxColon mempty
                  pure (forIdent, forExpr)
              (eForBlock, forBlockInfo, forBlockFocus) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  (Maybe.fromMaybe Trie.empty . Trie.down For_Block <$> dErrors)
                  versioned
                  (case focus of
                      Focus (Cons For_Block focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path For_Block)
                  body
              pure
                ( leftmost [eForIdent, eForExpr, eForBlock]
                , forIdentInfo <> forExprInfo <> forBlockInfo
                , forIdentFocus <|> forExprFocus <|> forBlockFocus
                )
            NIfThen cond then_ -> do
              (eIfThenCond, ifThenCondInfo, ifThenCondFocus) <-
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "if"
                  ifThenCond <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      (Maybe.fromMaybe Trie.empty . Trie.down IfThen_Cond <$> dErrors)
                      versioned
                      (case focus of
                        Focus (Cons IfThen_Cond focusPath) -> Focus focusPath
                        _ -> NoFocus
                      )
                      (Path.snoc path IfThen_Cond)
                      cond
                  syntaxColon mempty
                  pure ifThenCond
              (eIfThenThen, ifThenThenInfo, ifThenThenFocus) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  (Maybe.fromMaybe Trie.empty . Trie.down IfThen_Then <$> dErrors)
                  versioned
                  (case focus of
                      Focus (Cons IfThen_Then focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path IfThen_Then)
                  then_
              pure
                ( leftmost [eIfThenCond, eIfThenThen]
                , ifThenCondInfo <> ifThenThenInfo
                , ifThenCondFocus <|> ifThenThenFocus
                )
            NIfThenElse cond then_ else_ -> do
              (eIfThenElseCond, ifThenElseCondInfo, ifThenElseCondFocus) <-
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "if"
                  ifThenElseCond <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      (Maybe.fromMaybe Trie.empty . Trie.down IfThenElse_Cond <$> dErrors)
                      versioned
                      (case focus of
                        Focus (Cons IfThenElse_Cond focusPath) -> Focus focusPath
                        _ -> NoFocus
                      )
                      (Path.snoc path IfThenElse_Cond)
                      cond
                  syntaxColon mempty
                  pure ifThenElseCond
              (eIfThenElseThen, ifThenElseThenInfo, ifThenElseThenFocus) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  (Maybe.fromMaybe Trie.empty . Trie.down IfThenElse_Then <$> dErrors)
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
              (eIfThenElseElse, ifThenElseElseInfo, ifThenElseElseFocus) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  (Maybe.fromMaybe Trie.empty . Trie.down IfThenElse_Else <$> dErrors)
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
                , ifThenElseCondFocus <|> ifThenElseThenFocus <|> ifThenElseElseFocus
                )
            NPrint val ->
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "print"
                syntaxColon mempty
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  (Maybe.fromMaybe Trie.empty . Trie.down Print_Value <$> dErrors)
                  versioned
                  (case focus of
                      Focus (Cons Print_Value focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path Print_Value)
                  val
            NReturn val ->
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "return"
                syntaxColon mempty
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  (Maybe.fromMaybe Trie.empty . Trie.down Return_Value <$> dErrors)
                  versioned
                  (case focus of
                      Focus (Cons Return_Value focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path Return_Value)
                  val
            NDef name args body -> do
              ((eDefName, defNameInfo, defNameFocus), (eDefArgs, defArgsInfo, defArgsFocus)) <-
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "def"
                  defName <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      (Maybe.fromMaybe Trie.empty . Trie.down Def_Name <$> dErrors)
                      versioned
                      (case focus of
                          Focus (Cons Def_Name focusPath) -> Focus focusPath
                          _ -> NoFocus
                      )
                      (Path.snoc path Def_Name)
                      name
                  defArgs <-
                    renderNodeHash
                      contextMenuControls
                      controls
                      dMenu
                      (Maybe.fromMaybe Trie.empty . Trie.down Def_Args <$> dErrors)
                      versioned
                      (case focus of
                          Focus (Cons Def_Args focusPath) -> Focus focusPath
                          _ -> NoFocus
                      )
                      (Path.snoc path Def_Args)
                      args
                  syntaxColon mempty
                  pure (defName, defArgs)
              (eDefBody, defBodyInfo, defBodyFocus) <-
                syntaxNested mempty $
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  (Maybe.fromMaybe Trie.empty . Trie.down Def_Body <$> dErrors)
                  versioned
                  (case focus of
                      Focus (Cons Def_Body focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path Def_Body)
                  body
              pure
                ( leftmost [eDefName, eDefArgs, eDefBody]
                , defNameInfo <> defArgsInfo <> defBodyInfo
                , defNameFocus <|> defArgsFocus <|> defBodyFocus
                )
            NBool b ->
              ((never, mempty, Nothing) <$) . syntaxLiteral mempty . Dom.text $
              if b then "true" else "false"
            NInt n ->
              ((never, mempty, Nothing) <$) . syntaxLiteral mempty . Dom.text $
              Text.pack (show n)
            NBinOp op left right ->
              syntaxInline mempty $ do
                (eBinOpLeft, binOpLeftInfo, binOpLeftFocus) <-
                  renderNodeHash
                    contextMenuControls
                    controls
                    dMenu
                    (Maybe.fromMaybe Trie.empty . Trie.down BinOp_Left <$> dErrors)
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
                  Eq -> syntaxSymbol mempty $ Dom.text "=="
                  And -> syntaxKeyword mempty $ Dom.text "and"
                  Or -> syntaxKeyword mempty $ Dom.text "or"
                (eBinOpRight, binOpRightInfo, binOpRightFocus) <-
                  renderNodeHash
                    contextMenuControls
                    controls
                    dMenu
                    (Maybe.fromMaybe Trie.empty . Trie.down BinOp_Right <$> dErrors)
                    versioned
                    (case focus of
                        Focus (Cons BinOp_Right focusPath) -> Focus focusPath
                        _ -> NoFocus
                    )
                    (Path.snoc path BinOp_Right)
                    right
                pure
                  ( leftmost [eBinOpLeft, eBinOpRight]
                  , binOpLeftInfo <> binOpRightInfo
                  , binOpLeftFocus <|> binOpRightFocus
                  )
            NUnOp op val ->
              syntaxInline mempty $ do
                case op of
                  Neg -> syntaxSymbol mempty $ Dom.text "-"
                  Not -> syntaxKeyword mempty $ Dom.text "not"
                renderNodeHash
                  contextMenuControls
                  controls
                  dMenu
                  (Maybe.fromMaybe Trie.empty . Trie.down UnOp_Value <$> dErrors)
                  versioned
                  (case focus of
                      Focus (Cons UnOp_Value focusPath) -> Focus focusPath
                      _ -> NoFocus
                  )
                  (Path.snoc path UnOp_Value)
                  val
            NCall func args ->
              syntaxInline mempty $ do
                (eCallFunc, callFuncInfo, callFuncFocus) <-
                  renderNodeHash
                    contextMenuControls
                    controls
                    dMenu
                    (Maybe.fromMaybe Trie.empty . Trie.down Call_Function <$> dErrors)
                    versioned
                    (case focus of
                        Focus (Cons Call_Function focusPath) -> Focus focusPath
                        _ -> NoFocus
                    )
                    (Path.snoc path Call_Function)
                    func
                (eCallArgs, callArgsInfo, callArgsFocus) <-
                  renderNodeHash
                    contextMenuControls
                    controls
                    dMenu
                    (Maybe.fromMaybe Trie.empty . Trie.down Call_Args <$> dErrors)
                    versioned
                    (case focus of
                        Focus (Cons Call_Args focusPath) -> Focus focusPath
                        _ -> NoFocus
                    )
                    (Path.snoc path Call_Args)
                    args
                pure
                  ( leftmost [eCallFunc, eCallArgs]
                  , callFuncInfo <> callArgsInfo
                  , callFuncFocus <|> callArgsFocus
                  )
            NBlock sts -> do
              nodes <-
                traverse
                  (\(ix, st) ->
                     renderNodeHash
                       contextMenuControls
                       controls
                       dMenu
                       (Maybe.fromMaybe Trie.empty . Trie.down (Block_Index ix) <$> dErrors)
                       versioned
                       (case focus of
                          Focus (Cons (Block_Index ix') focus') | ix == ix' ->
                            Focus focus'
                          _ -> NoFocus
                       )
                       (Path.snoc path $ Block_Index ix)
                       st
                  )
                  (zip [0::Int ..] $ NonEmpty.toList sts)
              pure
                ( leftmost $ (\(a, _, _) -> a) <$> nodes
                , foldMap (\(_, a, _) -> a) nodes
                , asum $ (\(_, _, a) -> a) <$> nodes
                )
            NArgs xs -> do
              nodes <-
                syntaxInline mempty $ do
                  syntaxLParen mempty
                  nodes <-
                    traverse
                      (\item ->
                        case item of
                          Nothing ->
                            (never, mempty, Nothing) <$ syntaxComma mempty
                          Just (ix, x) ->
                            renderNodeHash
                              contextMenuControls
                              controls
                              dMenu
                              (Maybe.fromMaybe Trie.empty . Trie.down (Args_Index ix) <$> dErrors)
                              versioned
                              (case focus of
                                  Focus (Cons (Args_Index ix') focus') | ix == ix' ->
                                    Focus focus'
                                  _ -> NoFocus
                              )
                              (Path.snoc path $ Args_Index ix)
                              x
                      )
                      (List.intersperse Nothing $ Just <$> zip [0::Int ..] xs)
                  syntaxRParen mempty
                  pure nodes
              pure
                ( leftmost $ (\(a, _, _) -> a) <$> nodes
                , foldMap (\(_, a, _) -> a) nodes
                , asum $ (\(_, _, a) -> a) <$> nodes
                )
            NParams xs -> do
              nodes <-
                syntaxInline mempty $ do
                  syntaxLParen mempty
                  nodes <-
                    traverse
                      (\item ->
                        case item of
                          Nothing ->
                            (never, mempty, Nothing) <$ syntaxComma mempty
                          Just (ix, x) ->
                            renderNodeHash
                              contextMenuControls
                              controls
                              dMenu
                              (Maybe.fromMaybe Trie.empty . Trie.down (Params_Index ix) <$> dErrors)
                              versioned
                              (case focus of
                                  Focus (Cons (Params_Index ix') focus') | ix == ix' ->
                                    Focus focus'
                                  _ -> NoFocus
                              )
                              (Path.snoc path $ Params_Index ix)
                              x
                      )
                      (List.intersperse Nothing $ Just <$> zip [0::Int ..] xs)
                  syntaxRParen mempty
                  pure nodes
              pure
                ( leftmost $ (\(a, _, _) -> a) <$> nodes
                , foldMap (\(_, a, _) -> a) nodes
                , asum $ (\(_, _, a) -> a) <$> nodes
                )
            NSHole ->
              (never, mempty, Nothing) <$ syntaxHole mempty
            NEHole ->
              (never, mempty, Nothing) <$ syntaxHole mempty
