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
  ( NodeControls(..), NodeEvent(..), NodeInfo(..), FocusedNode(..)
  , RenderNodeEnv(..)
  , RenderNodeInfo(..)
  , rniNodeEvent
  , rniNodeInfo
  , rniFocusElement
  , rniFocusNode
  , renderNodeHash
  , syntaxNode
  , syntaxNode'
  )
where

import Control.Applicative ((<|>), liftA2)
import Control.Lens.Getter ((^.), view, views)
import Control.Lens.Setter (ASetter, (.~))
import Control.Lens.TH (makeClassy, makeLenses)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
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

dynSyntaxNodeD' ::
  (DomBuilder t m, PostBuild t m, MonadHold t m) =>
  Dynamic t Attrs ->
  Dynamic t (m a) ->
  m (Dynamic t (Dom.Element Dom.EventResult (DomBuilderSpace m) t, a))
dynSyntaxNodeD' attrs dInput =
  Dom.widgetHold
    (syntaxNodeD' attrs =<< sample (current dInput))
    (syntaxNodeD' attrs <$> updated dInput)

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

syntaxLBracket :: DomBuilder t m => Attrs -> m ()
syntaxLBracket attrs = syntaxSymbol ("class" =: "syntax-paren" <> attrs) $ Dom.text "["

syntaxRBracket :: DomBuilder t m => Attrs -> m ()
syntaxRBracket attrs = syntaxSymbol ("class" =: "syntax-paren" <> attrs) $ Dom.text "]"

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
    NList{} -> False
    NExprs{} -> False
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
  { _niHovered :: Dynamic t Bool
  }
makeLenses ''NodeInfo

instance Reflex t => Semigroup (NodeInfo t) where
  a <> b = NodeInfo { _niHovered = (||) <$> _niHovered a <*> _niHovered b }

instance Reflex t => Monoid (NodeInfo t) where
  mempty = NodeInfo { _niHovered = pure False }

data FocusedNode a where
  FocusedNode :: KnownNodeType b => Path a b -> Hash b -> Node b -> FocusedNode a

data RenderNodeInfo t a
  = RenderNodeInfo
  { _rniNodeEvent :: Event t (NodeEvent a)
  , _rniNodeInfo :: NodeInfo t
  , _rniFocusNode :: Dynamic t (Maybe (FocusedNode a))
  , _rniFocusElement :: Dynamic t (Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t))
  }
makeLenses ''RenderNodeInfo
instance Reflex t => Semigroup (RenderNodeInfo t a) where
  rni1 <> rni2 =
    RenderNodeInfo
    { _rniNodeEvent = leftmost [rni1 ^. rniNodeEvent, rni2 ^. rniNodeEvent]
    , _rniNodeInfo = rni1 ^. rniNodeInfo <> rni2 ^. rniNodeInfo
    , _rniFocusNode = liftA2 (<|>) (rni1 ^. rniFocusNode) (rni2 ^. rniFocusNode)
    , _rniFocusElement = liftA2 (<|>) (rni1 ^. rniFocusElement) (rni2 ^. rniFocusElement)
    }
instance Reflex t => Monoid (RenderNodeInfo t a) where
  mempty =
    RenderNodeInfo
    { _rniNodeEvent = never
    , _rniNodeInfo = mempty
    , _rniFocusNode = pure Nothing
    , _rniFocusElement = pure Nothing
    }

data RenderNodeEnv t a b
  = RenderNodeEnv
  { _rnContextMenuControls :: ContextMenuControls t
  , _rnNodeControls :: NodeControls t
  , _rnMenu :: Dynamic t Menu
  , _rnErrors :: Dynamic t (Trie b CheckError)
  , _rnVersioned :: Dynamic t (Versioned a)
  , _rnFocus :: Dynamic t (Focus b)
  , _rnPath :: Path a b
  }
makeClassy ''RenderNodeEnv

scribeDyn ::
  forall timeline s m t a b.
  (Reflex timeline, DynamicWriter timeline t m, Monoid s) =>
  ASetter s t a b ->
  Dynamic timeline b ->
  m ()
scribeDyn l dB =
  tellDyn $ (\b -> mempty & l .~ b) <$> dB

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

  , DynamicWriter t (RenderNodeInfo t a) m
  ) =>
  Hash b ->
  m ()
renderNodeHash h = do
  dError :: Dynamic t (Maybe (CheckError b)) <- views rnErrors $ fmap Trie.current

  dInFocus <-
    holdUniqDyn =<<
    views rnFocus (fmap (\case; Focus Nil -> True; _ -> False))

  rec
    let
      eMouseenter = switchDyn $ Dom.domEvent Dom.Mouseenter <$> dNodeElement
      eMouseleave = switchDyn $ Dom.domEvent Dom.Mouseleave <$> dNodeElement
      eMousedown = switchDyn $ Dom.domEvent Dom.Mousedown <$> dNodeElement

      dChildHovered =
        dRenderNodeInfo >>= view (rniNodeInfo.niHovered)

    dMouseInside <-
      case nodeType @b of
        TBlock -> pure dChildHovered
        _ -> holdDyn False $ leftmost [True <$ eMouseenter, False <$ eMouseleave]
    dHovered <-
      case nodeType @b of
        TBlock -> pure $ constDyn False
        _ ->
          holdUniqDyn $
          (\inFocus inside children -> inside && not children && not inFocus) <$>
          dInFocus <*>
          dMouseInside <*>
          dChildHovered

    let eClicked = gate (current dHovered) eMousedown

    dVersioned <- view rnVersioned
    let
      dmNode =
        (\versioned ->
            let
              Identity (mNode, _) = runVersionedT versioned $ Store.lookupNode h
            in
              (,) h <$> mNode
        ) <$>
        dVersioned
    (dNodeElement, dRenderNodeInfo) <-
      runDynamicWriterT $
      renderNode dInFocus dError dHovered dmNode

  let
    nodeInfo =
      NodeInfo
      { _niHovered = dMouseInside
      }

  dMenu <- view rnMenu
  controls <- view rnNodeControls
  let
    eOpenMenu =
      attachWithMaybe
        (\(inFocus, menu) () -> if inFocus && menu == MenuClosed then Just OpenMenu else Nothing)
        ((,) <$> current dInFocus <*> current dMenu)
        (ncOpenMenu controls)

  tellDyn dRenderNodeInfo

  -- why do I get an overlapping instance error without the type applications?
  scribeDyn @t @(RenderNodeInfo t a) rniNodeEvent $ pure eOpenMenu
  scribeDyn @t @(RenderNodeInfo t a) rniNodeEvent . pure $
    attachWithMaybe
      (\menu () -> if menu == MenuOpen then Just CloseMenu else Nothing)
      (current dMenu)
      (ncCloseMenu controls)
  path <- view rnPath
  scribeDyn @t @(RenderNodeInfo t a) rniNodeEvent . pure $
    Select path <$ eClicked

  scribeDyn @t @(RenderNodeInfo t a) rniNodeInfo $ pure nodeInfo

  scribeDyn @t @(RenderNodeInfo t a) rniFocusElement . pure $
    (\nodeElement inFocus -> if inFocus then Just nodeElement else Nothing) <$>
    dNodeElement <*>
    dInFocus

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
      view rnFocus env <&>
      \case
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

  , DynamicWriter t (RenderNodeInfo t a) m
  ) =>
  Dynamic t Bool ->
  Dynamic t (Maybe (CheckError b)) ->
  Dynamic t Bool ->
  Dynamic t (Maybe (Hash b, Node b)) ->
  m (Dynamic t (Dom.Element Dom.EventResult GhcjsDomSpace t))
renderNode dInFocus dError dHovered dmNode = do
  path <- view rnPath
  let
    dAttrs =
      fmap
        (\mNode ->
            (if Maybe.maybe False isHole (snd <$> mNode)
             then "class" =: "syntax-hole"
             else mempty
            ) <>
            case nodeType @b of
              TExpr -> "class" =: "syntax-expr"
              TBlock -> "class" =: "syntax-block"
              TStatement -> "class" =: "syntax-statement"
              TIdent -> "class" =: "syntax-ident"
              TArgs -> "class" =: "syntax-args"
              TParams -> "class" =: "syntax-params"
              TExprs -> "class" =: "syntax-exprs"
        )
        dmNode <>
      fmap (\hovered -> if hovered then "class" =: "syntax-hovered" else mempty) dHovered <>
      fmap (\mError -> case mError of; Nothing -> mempty; Just _ -> "class" =: "has-error") dError <>
      fmap (\inFocus -> if inFocus then "class" =: "syntax-focused" else mempty) dInFocus
  dmNodeFactored <- maybeDyn dmNode
  (fmap.fmap) fst . dynSyntaxNodeD' dAttrs $
    dmNodeFactored <&>
    \mdNode ->
    case mdNode of
      Nothing ->
        Dom.text "error: missing node"
      Just dNode -> do
        scribeDyn @t @(RenderNodeInfo t a) rniFocusNode . pure $
          (\inFocus (hash, node) ->
             if inFocus
             then Just $ FocusedNode path hash node
             else Nothing
          ) <$>
          dInFocus <*>
          dNode

        Dom.dyn_ @_ @_ @() $
          dNode <&> \(_, node) ->
          case node of
            NEIdent n ->
              Dom.text (Text.pack n)
            NIdent n ->
              Dom.text (Text.pack n)
            NIHole ->
              syntaxHole mempty
            NFor ident val body -> do
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "for"
                down
                  (\case; For_Ident -> Just (Refl, Refl); _ -> Nothing)
                  For_Ident
                  (renderNodeHash ident)
                syntaxKeyword mempty $ Dom.text "in"
                down
                  (\case; For_Expr -> Just (Refl, Refl); _ -> Nothing)
                  For_Expr
                  (renderNodeHash val)
                syntaxColon mempty
              syntaxNested mempty $
                down
                  (\case; For_Block -> Just (Refl, Refl); _ -> Nothing)
                  For_Block
                  (renderNodeHash body)
            NIfThen cond then_ -> do
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "if"
                down
                  (\case; IfThen_Cond -> Just (Refl, Refl); _ -> Nothing)
                  IfThen_Cond
                  (renderNodeHash cond)
                syntaxColon mempty
              syntaxNested mempty $
                down
                  (\case; IfThen_Then -> Just (Refl, Refl); _ -> Nothing)
                  IfThen_Then
                  (renderNodeHash then_)
            NIfThenElse cond then_ else_ -> do
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "if"
                down
                  (\case; IfThenElse_Cond -> Just (Refl, Refl); _ -> Nothing)
                  IfThenElse_Cond
                  (renderNodeHash cond)
                syntaxColon mempty
              syntaxNested mempty $
                down
                  (\case; IfThenElse_Then -> Just (Refl, Refl); _ -> Nothing)
                  IfThenElse_Then
                  (renderNodeHash then_)
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "else"
                syntaxColon mempty
              syntaxNested mempty $
                down
                  (\case; IfThenElse_Else -> Just (Refl, Refl); _ -> Nothing)
                  IfThenElse_Else
                  (renderNodeHash else_)
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
              syntaxLine mempty $ do
                syntaxKeyword mempty $ Dom.text "def"
                down
                  (\case; Def_Name -> Just (Refl, Refl); _ -> Nothing)
                  Def_Name
                  (renderNodeHash name)
                down
                  (\case; Def_Args -> Just (Refl, Refl); _ -> Nothing)
                  Def_Args
                  (renderNodeHash args)
                syntaxColon mempty
              syntaxNested mempty $
                down
                  (\case; Def_Body -> Just (Refl, Refl); _ -> Nothing)
                  Def_Body
                  (renderNodeHash body)
            NBool b ->
              syntaxLiteral mempty . Dom.text $
              if b then "true" else "false"
            NInt n ->
              syntaxLiteral mempty . Dom.text $
              Text.pack (show n)
            NBinOp op left right ->
              syntaxInline mempty $ do
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
                down
                  (\case; BinOp_Right -> Just (Refl, Refl); _ -> Nothing)
                  BinOp_Right
                  (renderNodeHash right)
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
                down
                  (\case; Call_Function -> Just (Refl, Refl); _ -> Nothing)
                  Call_Function
                  (renderNodeHash func)
                syntaxLParen mempty
                down
                  (\case; Call_Args -> Just (Refl, Refl); _ -> Nothing)
                  Call_Args
                  (renderNodeHash args)
                syntaxRParen mempty
            NList exprs -> do
              syntaxInline mempty $ do
                syntaxLBracket mempty
                down
                  (\case; List_Exprs -> Just (Refl, Refl); _ -> Nothing)
                  List_Exprs
                  (renderNodeHash exprs)
                syntaxRBracket mempty
            NExprs xs ->
              syntaxInline mempty $
              traverse_
                (\item ->
                  case item of
                    Nothing ->
                      syntaxComma mempty
                    Just (ix, x) ->
                      down
                        (\case; Exprs_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                        (Exprs_Index ix)
                        (renderNodeHash x)
                )
                (List.intersperse Nothing $ Just <$> zip [0::Int ..] xs)
            NBlock sts -> do
              traverse_
                (\(ix, st) ->
                  down
                    (\case; Block_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                    (Block_Index ix)
                    (renderNodeHash st)
                )
                (zip [0::Int ..] $ NonEmpty.toList sts)
            NArgs xs -> do
              syntaxInline mempty $ do
                traverse_
                  (\item ->
                    case item of
                      Nothing ->
                        syntaxComma mempty
                      Just (ix, x) ->
                        down
                          (\case; Args_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                          (Args_Index ix)
                          (renderNodeHash x)
                  )
                  (List.intersperse Nothing $ Just <$> zip [0::Int ..] xs)
            NParams xs -> do
              syntaxInline mempty $ do
                syntaxLParen mempty
                traverse_
                  (\item ->
                    case item of
                      Nothing ->
                        syntaxComma mempty
                      Just (ix, x) ->
                        down
                          (\case; Params_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                          (Params_Index ix)
                          (renderNodeHash x)
                  )
                  (List.intersperse Nothing $ Just <$> zip [0::Int ..] xs)
                syntaxRParen mempty
            NSHole ->
              syntaxHole mempty
            NEHole ->
              syntaxHole mempty
