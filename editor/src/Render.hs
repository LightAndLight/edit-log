{-# language FlexibleContexts #-}
{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module Render
  ( NodeControls(..), NodeEvent(..), NodeInfo(..)
  , RenderNodeEnv(..)
  , renderNodeHash
  , syntaxNode
  , syntaxNode'
  )
where

import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.), view, views)
import Control.Lens.TH (makeClassy)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Data.Foldable (asum)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Type.Equality ((:~:)(..))
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

data RenderNodeEnv t a b
  = RenderNodeEnv
  { _rnContextMenuControls :: ContextMenuControls t
  , _rnNodeControls :: NodeControls t
  , _rnMenu :: Dynamic t Menu
  , _rnErrors :: Dynamic t (Trie b CheckError)
  , _rnVersioned :: Versioned a
  , _rnFocus :: Focus b
  , _rnPath :: Path a b
  }
makeClassy ''RenderNodeEnv

renderNodeHash ::
  forall t m a b r.
  ( MonadHold t m, DomBuilder t m, PostBuild t m
  , PerformEvent t m, MonadJSM (Performable m)
  , DomBuilderSpace m ~ GhcjsDomSpace
  , TriggerEvent t m
  , MonadFix m, MonadJSM m
  , KnownNodeType b
  , HasRenderNodeEnv r t a b
  , MonadReader r m
  ) =>
  Hash b ->
  m
    ( Event t (NodeEvent a)
    , NodeInfo t
    , Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t)
    )
renderNodeHash h = do
  dError :: Dynamic t (Maybe (CheckError b)) <- views rnErrors $ fmap Trie.current

  inFocus <- views rnFocus $ \case; Focus Nil -> True; _ -> False

  rec
    let
      eMouseenter = Dom.domEvent Dom.Mouseenter nodeElement
      eMouseleave = Dom.domEvent Dom.Mouseleave nodeElement
      eMousedown = Dom.domEvent Dom.Mousedown nodeElement

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

    versioned <- view rnVersioned
    let Identity (mNode, _) = runVersionedT versioned $ Store.lookupNode h
    (nodeElement, (eChildren, childrenInfo, childFocus)) <-
      renderNode inFocus dError dHovered mNode

  let
    nodeInfo =
      NodeInfo
      { niHovered = dMouseInside
      }

  dMenu <- view rnMenu
  controls <- view rnNodeControls
  let
    eOpenMenu =
      attachWithMaybe
        (\menu () -> if inFocus && menu == MenuClosed then Just OpenMenu else Nothing)
        (current dMenu)
        (ncOpenMenu controls)

  path <- view rnPath
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

down ::
  ( Monad m
  , Reflex t
  , HasRenderNodeEnv r t a b
  , MonadReader r m
  ) =>
  (forall x y. Level x y -> Maybe (x :~: b, y :~: b')) ->
  Level b b' ->
  ReaderT (RenderNodeEnv t a b') m res ->
  m res
down match build m = do
  env <- ask
  runReaderT m $
    RenderNodeEnv
    { _rnContextMenuControls = env ^. rnContextMenuControls
    , _rnNodeControls = env ^. rnNodeControls
    , _rnMenu = env ^. rnMenu
    , _rnErrors = Maybe.fromMaybe Trie.empty . Trie.down build <$> view rnErrors env
    , _rnVersioned = env ^. rnVersioned
    , _rnFocus =
      case view rnFocus env of
        Focus (Cons level focusPath) | Just (Refl, Refl) <- match level ->
          Focus focusPath
        _ ->
          NoFocus
    , _rnPath = Path.snoc (view rnPath env) build
    }

renderNode ::
  forall t m a b r.
  ( MonadHold t m, DomBuilder t m, PostBuild t m
  , PerformEvent t m, MonadJSM (Performable m)
  , DomBuilderSpace m ~ GhcjsDomSpace
  , TriggerEvent t m
  , MonadFix m, MonadJSM m
  , KnownNodeType b
  , HasRenderNodeEnv r t a b
  , MonadReader r m
  ) =>
  Bool ->
  Dynamic t (Maybe (CheckError b)) ->
  Dynamic t Bool ->
  Maybe (Node b) ->
  m
    ( Dom.Element Dom.EventResult GhcjsDomSpace t
    , (Event t (NodeEvent a), NodeInfo t, Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t))
    )
renderNode inFocus dError dHovered mNode =
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
                    down
                      (\case; For_Ident -> Just (Refl, Refl); _ -> Nothing)
                      For_Ident
                      (renderNodeHash ident)
                  syntaxKeyword mempty $ Dom.text "in"
                  forExpr <-
                    down
                      (\case; For_Expr -> Just (Refl, Refl); _ -> Nothing)
                      For_Expr
                      (renderNodeHash val)
                  syntaxColon mempty
                  pure (forIdent, forExpr)
              (eForBlock, forBlockInfo, forBlockFocus) <-
                syntaxNested mempty $
                down
                  (\case; For_Block -> Just (Refl, Refl); _ -> Nothing)
                  For_Block
                  (renderNodeHash body)
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
                    down
                      (\case; IfThen_Cond -> Just (Refl, Refl); _ -> Nothing)
                      IfThen_Cond
                      (renderNodeHash cond)
                  syntaxColon mempty
                  pure ifThenCond
              (eIfThenThen, ifThenThenInfo, ifThenThenFocus) <-
                syntaxNested mempty $
                down
                  (\case; IfThen_Then -> Just (Refl, Refl); _ -> Nothing)
                  IfThen_Then
                  (renderNodeHash then_)
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
                    down
                      (\case; IfThenElse_Cond -> Just (Refl, Refl); _ -> Nothing)
                      IfThenElse_Cond
                      (renderNodeHash cond)
                  syntaxColon mempty
                  pure ifThenElseCond
              (eIfThenElseThen, ifThenElseThenInfo, ifThenElseThenFocus) <-
                syntaxNested mempty $
                down
                  (\case; IfThenElse_Then -> Just (Refl, Refl); _ -> Nothing)
                  IfThenElse_Then
                  (renderNodeHash then_)
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "else"
                syntaxColon mempty
              (eIfThenElseElse, ifThenElseElseInfo, ifThenElseElseFocus) <-
                syntaxNested mempty $
                down
                  (\case; IfThenElse_Else -> Just (Refl, Refl); _ -> Nothing)
                  IfThenElse_Else
                  (renderNodeHash else_)
              pure
                ( leftmost [eIfThenElseCond, eIfThenElseThen, eIfThenElseElse]
                , ifThenElseCondInfo <> ifThenElseThenInfo <> ifThenElseElseInfo
                , ifThenElseCondFocus <|> ifThenElseThenFocus <|> ifThenElseElseFocus
                )
            NPrint val ->
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "print"
                syntaxColon mempty
                down
                  (\case; Print_Value -> Just (Refl, Refl); _ -> Nothing)
                  Print_Value
                  (renderNodeHash val)
            NReturn val ->
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "return"
                syntaxColon mempty
                down
                  (\case; Return_Value -> Just (Refl, Refl); _ -> Nothing)
                  Return_Value
                  (renderNodeHash val)
            NDef name args body -> do
              ((eDefName, defNameInfo, defNameFocus), (eDefArgs, defArgsInfo, defArgsFocus)) <-
                syntaxLine mempty $ do
                  syntaxKeyword mempty $ Dom.text "def"
                  defName <-
                    down
                      (\case; Def_Name -> Just (Refl, Refl); _ -> Nothing)
                      Def_Name
                      (renderNodeHash name)
                  defArgs <-
                    down
                      (\case; Def_Args -> Just (Refl, Refl); _ -> Nothing)
                      Def_Args
                      (renderNodeHash args)
                  syntaxColon mempty
                  pure (defName, defArgs)
              (eDefBody, defBodyInfo, defBodyFocus) <-
                syntaxNested mempty $
                down
                  (\case; Def_Body -> Just (Refl, Refl); _ -> Nothing)
                  Def_Body
                  (renderNodeHash body)
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
                  down
                    (\case; BinOp_Left -> Just (Refl, Refl); _ -> Nothing)
                    BinOp_Left
                    (renderNodeHash left)
                case op of
                  Add -> syntaxSymbol mempty $ Dom.text "+"
                  Sub -> syntaxSymbol mempty $ Dom.text "-"
                  Mul -> syntaxSymbol mempty $ Dom.text "*"
                  Div -> syntaxSymbol mempty $ Dom.text "/"
                  Eq -> syntaxSymbol mempty $ Dom.text "=="
                  And -> syntaxKeyword mempty $ Dom.text "and"
                  Or -> syntaxKeyword mempty $ Dom.text "or"
                (eBinOpRight, binOpRightInfo, binOpRightFocus) <-
                  down
                    (\case; BinOp_Right -> Just (Refl, Refl); _ -> Nothing)
                    BinOp_Right
                    (renderNodeHash right)
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
                down
                  (\case; UnOp_Value -> Just (Refl, Refl); _ -> Nothing)
                  UnOp_Value
                  (renderNodeHash val)
            NCall func args ->
              syntaxInline mempty $ do
                (eCallFunc, callFuncInfo, callFuncFocus) <-
                  down
                    (\case; Call_Function -> Just (Refl, Refl); _ -> Nothing)
                    Call_Function
                    (renderNodeHash func)
                (eCallArgs, callArgsInfo, callArgsFocus) <-
                  down
                    (\case; Call_Args -> Just (Refl, Refl); _ -> Nothing)
                    Call_Args
                    (renderNodeHash args)
                pure
                  ( leftmost [eCallFunc, eCallArgs]
                  , callFuncInfo <> callArgsInfo
                  , callFuncFocus <|> callArgsFocus
                  )
            NBlock sts -> do
              nodes <-
                traverse
                  (\(ix, st) ->
                    down
                      (\case; Block_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                      (Block_Index ix)
                      (renderNodeHash st)
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
                            down
                              (\case; Args_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                              (Args_Index ix)
                              (renderNodeHash x)
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
                            down
                              (\case; Params_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                              (Params_Index ix)
                              (renderNodeHash x)
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
