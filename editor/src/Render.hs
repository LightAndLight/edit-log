{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# options_ghc -fno-warn-overlapping-patterns #-}
module Render
  ( NodeControls(..), NodeEvent(..)
  , RenderNodeEnv(..)
  , ChildInfo(..)
  , Located(..)
  , renderNodeHash
  )
where

import Control.Applicative ((<|>))
import Control.Lens.Getter ((^.), view, views)
import Control.Lens.Indexed (itraverse_)
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import qualified Data.Dependent.Map as DMap
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(..))
import Data.GADT.Compare (GEq(..), GCompare(..), (:~:)(..), GOrdering(..))
import qualified Data.List as List
import qualified Data.Text as Text
import Reflex
import Reflex.Dom (DomBuilder, DomBuilderSpace, GhcjsDomSpace)
import qualified Reflex.Dom as Dom

import Hash (Hash)
import Node (Node(..))
import NodeType (KnownNodeType, NodeType(..), nodeType)
import Path (Level(..), Path(..))
import qualified Path
import qualified Store
import Syntax (BinOp(..), UnOp(..))
import Versioned.Pure (Versioned, runVersionedT)

import Attrs (Attrs, unAttrs, (=:))
import Focus (Focus(..))

data NodeControls t
  = NodeControls
  { _ncOpenMenu :: Event t ()
  , _ncCloseMenu :: Event t ()
  }
makeLenses ''NodeControls

data NodeEvent a where
  OpenMenu :: NodeEvent a
  CloseMenu :: NodeEvent a
  Select :: KnownNodeType b => Path a b -> NodeEvent a
instance Show a => Show (NodeEvent a) where
  showsPrec _ OpenMenu = showString "OpenMenu"
  showsPrec _ CloseMenu = showString "CloseMenu"
  showsPrec d (Select p) =
    showParen (d > 10) $
    showString "Select " .
    showsPrec 11 p

instance Semigroup (NodeEvent a) where
  l <> _ = l

data Change a b x where
  NewVersioned :: Change a b (Versioned a)
  NewHash :: Change a b (Hash b)
  NewFocus :: Change a b (Focus b)

instance GEq (Change a b) where
  geq NewVersioned NewVersioned = Just Refl
  geq NewVersioned _ = Nothing
  geq _ NewVersioned = Nothing

  geq NewHash NewHash = Just Refl
  geq NewHash _ = Nothing
  geq _ NewHash = Nothing

  geq NewFocus NewFocus = Just Refl
  geq NewFocus _ = Nothing
  geq _ NewFocus = Nothing

instance GCompare (Change a b) where
  gcompare NewVersioned NewVersioned = GEQ
  gcompare NewVersioned _ = GLT
  gcompare _ NewVersioned = GGT

  gcompare NewHash NewHash = GEQ
  gcompare NewHash _ = GLT
  gcompare _ NewHash = GGT

  gcompare NewFocus NewFocus = GEQ
  gcompare NewFocus _ = GLT
  gcompare _ NewFocus = GGT

data Located a f where
  Located :: Path a b -> f b -> Located a f

data RenderNodeEnv t a b
  = RenderNodeEnv
  { _rnVersioned :: Behavior t (Versioned a)
  , _rnVersionedChanged :: Event t (Versioned a)
  , _rnPath :: Path a b
  , _rnFocus :: Dynamic t (Focus b)
  , _rnHashChanged :: Event t (Located b Hash)
  , _rnNodeControls :: NodeControls t
  }
makeLenses ''RenderNodeEnv

data ChildInfo t
  = ChildInfo
  { _ciAnyMouseOver :: Dynamic t Bool
  , _ciFocusElement :: Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t)
  }

renderNodeHash ::
  forall t m a b.
  ( MonadHold t m, Adjustable t m
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , MonadReader (RenderNodeEnv t a b) m
  , EventWriter t (NodeEvent a) m
  , DynamicWriter t (ChildInfo t) m
  , PostBuild t m
  , KnownNodeType b
  , MonadFix m
  ) =>
  Versioned a ->
  Hash b ->
  m ()
renderNodeHash versioned hash = do
  rec
    eHash <-
      attachWithMaybe
        (\old located ->
           case located of
             Located Nil new | old /= new ->
               Just new
             _ ->
               Nothing
        )
        bHash <$>
      view rnHashChanged
    bHash <- hold hash eHash

  eVersioned <- view rnVersionedChanged
  bVersioned <- view rnVersioned

  Dom.widgetHold_
    (renderMaybeNode versioned $ getNode versioned hash)
    (attachWithMaybe
       (\v change -> do
          (v', h') <-
            case (DMap.lookup NewVersioned change, DMap.lookup NewHash change) of
              (_, Nothing) ->
                Nothing
              (Nothing, Just (Identity h)) ->
                pure (v, h)
              (Just (Identity v'), Just (Identity h)) ->
                pure (v', h)
          pure $ renderMaybeNode v' (getNode v' h')
       )
       bVersioned
       (mergeWith (<>)
         [ DMap.singleton NewVersioned . pure <$> eVersioned
         , DMap.singleton NewHash . pure <$> eHash
         ]
       )
    )
  where
    getNode :: Versioned a -> Hash b -> Maybe (Node b)
    getNode v h =
      let
        Identity (mNode, _) = runVersionedT v (Store.lookupNode h)
      in
        mNode

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

renderMaybeNode ::
  forall t m a b.
  ( MonadHold t m, Adjustable t m
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , MonadReader (RenderNodeEnv t a b) m
  , EventWriter t (NodeEvent a) m
  , DynamicWriter t (ChildInfo t) m
  , PostBuild t m
  , KnownNodeType b
  , MonadFix m
  ) =>
  Versioned a ->
  Maybe (Node b) ->
  m ()
renderMaybeNode versioned mNode =
  case mNode of
    Nothing ->
      Dom.text "error: missing node"
    Just node -> do
      let
        mkAttrs focused hovered =
          ("class" =: "syntax-node") <>
          (if focused then "class" =: "syntax-focused" else mempty) <>
          (if isHole node then "class" =: "syntax-hole" else mempty) <>
          (case nodeType @b of
            TExpr -> "class" =: "syntax-expr"
            TBlock -> "class" =: "syntax-block"
            TStatement -> "class" =: "syntax-statement"
            TIdent -> "class" =: "syntax-ident"
            TArgs -> "class" =: "syntax-args"
            TParams -> "class" =: "syntax-params"
            TExprs -> "class" =: "syntax-exprs"
          ) <>
          (if hovered && not focused then "class" =: "syntax-hovered" else mempty)

      let isFocused = \case; Focus Nil -> True; _ -> False
      dFocused <- views rnFocus $ fmap isFocused

      rec
        let
          eMouseenter = Dom.domEvent Dom.Mouseenter element
          eMouseleave = Dom.domEvent Dom.Mouseleave element
          dMouseOverChild = dChildInfo >>= _ciAnyMouseOver

        dMouseOver <-
          holdDyn False (leftmost [True <$ eMouseenter, False <$ eMouseleave])

        let
          dHovered =
            (\mouseOverChild mouseOverMe ->
               case nodeType @b of
                 TBlock -> False
                 _ -> not mouseOverChild && mouseOverMe
            ) <$>
            dMouseOverChild <*>
            dMouseOver

        let dAttrs = mkAttrs <$> dFocused <*> dHovered

        ((element, ()), dChildInfo) <-
          runDynamicWriterT .
          Dom.elDynAttr' "div" (fmap unAttrs dAttrs) $
          renderNode versioned node

      let eMousedown = Dom.domEvent Dom.Mousedown element

      tellDyn . pure $ mempty { _ciAnyMouseOver = dMouseOver }
      tellDyn $
        (\x -> (mempty @(ChildInfo t)) { _ciFocusElement = _ciFocusElement x }) <$>
        dChildInfo
      tellDyn $
        (\focused -> (mempty @(ChildInfo t)) { _ciFocusElement = if focused then Just element else Nothing }) <$>
        dFocused

      path <- view rnPath
      tellEvent $ Select path <$ gate (current dHovered) eMousedown

      eOpenMenu <- view $ rnNodeControls.ncOpenMenu
      tellEvent $ OpenMenu <$ gate (current dFocused) eOpenMenu

      eCloseMenu <- view $ rnNodeControls.ncCloseMenu
      tellEvent $ CloseMenu <$ gate (current dFocused) eCloseMenu

      pure ()

syntaxHole :: DomBuilder t m => Attrs -> m ()
syntaxHole attrs =
  Dom.elAttr "div" (unAttrs attrs) $
  Dom.text "?"

syntaxInline :: DomBuilder t m => Attrs -> m a -> m a
syntaxInline attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-inline" <> attrs

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

syntaxLine :: DomBuilder t m => Attrs -> m a -> m a
syntaxLine attrs = Dom.elAttr "div" . unAttrs $ "class" =: "syntax-line" <> attrs

downFocus ::
  (forall x. Level b x -> Maybe (x :~: c)) ->
  Focus b ->
  Focus c
downFocus match =
  \case
    NoFocus -> NoFocus
    Focus p ->
      case p of
        Nil -> NoFocus
        Cons l p' ->
          case match l of
            Nothing -> NoFocus
            Just Refl -> Focus p'

down ::
  forall t m a b c.
  (Reflex t, MonadReader (RenderNodeEnv t a b) m) =>
  Level b c ->
  (forall x. Level b x -> Maybe (x :~: c)) ->
  forall x.
  ReaderT (RenderNodeEnv t a c) m x ->
  m x
down level match m = do
  env <- ask
  let
    env' :: RenderNodeEnv t a c
    env' =
      RenderNodeEnv
      { _rnVersioned = env ^. rnVersioned
      , _rnVersionedChanged = env ^. rnVersionedChanged
      , _rnFocus = fmap (downFocus match) (env ^. rnFocus)
      , _rnPath = Path.snoc (env ^. rnPath) level
      , _rnHashChanged =
          fmapMaybe
            (\(Located p h) ->
               case p of
                 Nil -> Nothing
                 Cons l p' -> do
                   Refl <- match l
                   pure $ Located p' h
            )
            (env ^. rnHashChanged)
      , _rnNodeControls = env ^. rnNodeControls
      }
  runReaderT m env'

instance Reflex t => Semigroup (ChildInfo t) where
  c1 <> c2 =
    ChildInfo
    { _ciAnyMouseOver = (||) <$> _ciAnyMouseOver c1 <*> _ciAnyMouseOver c2
    , _ciFocusElement = _ciFocusElement c1 <|> _ciFocusElement c2
    }

instance Reflex t => Monoid (ChildInfo t) where
  mempty =
    ChildInfo
    { _ciAnyMouseOver = constDyn False
    , _ciFocusElement = Nothing
    }

renderNode ::
  ( MonadHold t m, Adjustable t m
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , MonadReader (RenderNodeEnv t a b) m
  , EventWriter t (NodeEvent a) m
  , DynamicWriter t (ChildInfo t) m
  , PostBuild t m
  , MonadFix m
  ) =>
  Versioned a ->
  Node b ->
  m ()
renderNode versioned node =
  case node of
    NEIdent n ->
      Dom.text $ Text.pack n
    NIdent n ->
      Dom.text $ Text.pack n
    NIHole ->
      syntaxHole mempty
    NFor ident val body -> do
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "for"
        down
          For_Ident
          (\case; For_Ident -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned ident)
        syntaxKeyword mempty $ Dom.text "in"
        down
          For_Expr
          (\case; For_Expr -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned val)
        syntaxColon mempty
      syntaxNested mempty $
        down
          For_Block
          (\case; For_Block -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned body)
    NIfThen cond then_ -> do
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "if"
        down
          IfThen_Cond
          (\case; IfThen_Cond -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned cond)
        syntaxColon mempty
      syntaxNested mempty $
        down
          IfThen_Then
          (\case; IfThen_Then -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned then_)
    NIfThenElse cond then_ else_ -> do
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "if"
        down
          IfThenElse_Cond
          (\case; IfThenElse_Cond -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned cond)
        syntaxColon mempty
      syntaxNested mempty $
        down
          IfThenElse_Then
          (\case; IfThenElse_Then -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned then_)
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "else"
        syntaxColon mempty
      syntaxNested mempty $
        down
          IfThenElse_Else
          (\case; IfThenElse_Else -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned else_)
    NPrint val ->
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "print"
        syntaxColon mempty
        down
          Print_Value
          (\case; Print_Value -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned val)
    NReturn val ->
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "return"
        syntaxColon mempty
        down
          Return_Value
          (\case; Return_Value -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned val)
    NDef name args body -> do
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "def"
        down
          Def_Name
          (\case; Def_Name -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned name)
        syntaxLParen mempty
        down
          Def_Args
          (\case; Def_Args -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned args)
        syntaxRParen mempty
        syntaxColon mempty
      syntaxNested mempty $
        down
          Def_Body
          (\case; Def_Body -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned body)
    NBool b ->
      syntaxLiteral mempty . Dom.text $ if b then "true" else "false"
    NInt n ->
      syntaxLiteral mempty . Dom.text $ Text.pack (show n)
    NBinOp op left right ->
      syntaxInline mempty $ do
        down
          BinOp_Left
          (\case; BinOp_Left -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned left)
        case op of
          Add -> syntaxSymbol mempty $ Dom.text "+"
          Sub -> syntaxSymbol mempty $ Dom.text "-"
          Mul -> syntaxSymbol mempty $ Dom.text "*"
          Div -> syntaxSymbol mempty $ Dom.text "/"
          Eq -> syntaxSymbol mempty $ Dom.text "=="
          And -> syntaxKeyword mempty $ Dom.text "and"
          Or -> syntaxKeyword mempty $ Dom.text "or"
        down
          BinOp_Right
          (\case; BinOp_Right -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned right)
    NUnOp op val ->
      syntaxInline mempty $ do
        case op of
          Neg -> syntaxSymbol mempty $ Dom.text "-"
          Not -> syntaxKeyword mempty $ Dom.text "not"
        down
          UnOp_Value
          (\case; UnOp_Value -> Just Refl; _ -> Nothing)
            (renderNodeHash versioned val)
    NCall func args ->
      syntaxInline mempty $ do
        down
          Call_Function
          (\case; Call_Function -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned func)
        syntaxLParen mempty
        down
          Call_Args
          (\case; Call_Args -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned args)
        syntaxRParen mempty
    NList exprs -> do
      syntaxInline mempty $ do
        syntaxLBracket mempty
        down
          List_Exprs
          (\case; List_Exprs -> Just Refl; _ -> Nothing)
          (renderNodeHash versioned exprs)
        syntaxRBracket mempty
    NExprs xs ->
      syntaxInline mempty $ do
        traverse_
          (\item ->
            case item of
              Nothing ->
                syntaxComma mempty
              Just (ix, x) ->
                down
                  (Exprs_Index ix)
                  (\case; Exprs_Index ix' | ix == ix' -> Just Refl; _ -> Nothing)
                  (renderNodeHash versioned x)
          ) .
          List.intersperse Nothing . fmap Just . zip [0::Int ..] $
          xs
    NBlock sts -> do
      itraverse_
        (\ix x ->
            down
              (Block_Index ix)
              (\case; Block_Index ix' | ix == ix' -> Just Refl; _ -> Nothing)
              (renderNodeHash versioned x)
        )
        sts
    NArgs xs -> do
      syntaxInline mempty $ do
        traverse_
          (\item ->
            case item of
              Nothing ->
                syntaxComma mempty
              Just (ix, x) ->
                down
                  (Args_Index ix)
                  (\case; Args_Index ix' | ix == ix' -> Just Refl; _ -> Nothing)
                  (renderNodeHash versioned x)
          ) .
          List.intersperse Nothing . fmap Just . zip [0::Int ..] $
          xs
    NParams xs -> do
      syntaxInline mempty $ do
        traverse_
          (\item ->
            case item of
              Nothing ->
                syntaxComma mempty
              Just (ix, x) ->
                down
                  (Params_Index ix)
                  (\case; Params_Index ix' | ix == ix' -> Just Refl; _ -> Nothing)
                  (renderNodeHash versioned x)
          ) .
          List.intersperse Nothing . fmap Just . zip [0::Int ..] $
          xs
    NSHole ->
      syntaxHole mempty
    NEHole ->
      syntaxHole mempty
