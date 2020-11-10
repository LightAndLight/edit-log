{-# language FlexibleContexts #-}
{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language GADTs, KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module Render
  ( NodeControls(..), NodeEvent(..), FocusedNode(..)
  , RenderNodeEnv(..)
  , RenderNodeInfo(..)
  , rniHovered
  , rniFocusElement
  , rniFocusNode
  , renderNodeHash
  , syntaxNode
  , syntaxNode'
  )
where

import Control.Monad.IO.Class

import Control.Applicative ((<|>))
import Control.Lens.Indexed (itraverse_)
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
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import GHCJS.DOM.Types (HTMLElement(..))
import qualified GHCJS.DOM.Types as GHCJS.DOM
import Language.Javascript.JSaddle.Monad (MonadJSM)
import Reflex hiding (maybeDyn)
import Reflex.Dom (DomBuilder, DomBuilderSpace, GhcjsDomSpace)
import qualified Reflex.Dom as Dom
import qualified Reflex.Network

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
import List.Dynamic (listDyn)
import Maybe.Dynamic (maybeDyn)
import Node.Dynamic (NodeD(..), nodeDyn)
import Svg (svgElAttr)

networkHold_ :: (Adjustable t m, MonadHold t m) => m a -> Event t (m a) -> m ()
networkHold_ child0 newChild = do
  (_, _) <- runWithReplace child0 newChild
  pure ()

svgUnderline ::
  (DomBuilder t m, PostBuild t m) =>
  (Int, Int) ->
  Int ->
  m ()
svgUnderline (x, y) width =
  svgElAttr "svg" (dimensions <> [("style", "position: absolute; left: " <> Text.pack (show x) <> "px; top: " <> Text.pack (show y) <> "px;")]) $
  svgElAttr "rect" (dimensions <> [("fill", "url(#zig)")]) $
  pure ()
  where
    dimensions = [("width", Text.pack $ show width), ("height", "2.5")]

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
instance Show a => Show (NodeEvent a) where
  showsPrec _ OpenMenu = showString "OpenMenu"
  showsPrec _ CloseMenu = showString "OpenMenu"
  showsPrec d (ContextMenuEvent e) =
    showParen (d > 10) $
    showString "ContextMenuEvent " .
    showsPrec 11 e
  showsPrec d (Select p) =
    showParen (d > 10) $
    showString "Select " .
    showsPrec 11 p

instance Semigroup (NodeEvent a) where
  l <> _ = l

data FocusedNode a where
  FocusedNode :: KnownNodeType b => Path a b -> Hash b -> Node b -> FocusedNode a
deriving instance Show (FocusedNode a)

data RenderNodeInfo t a
  = RenderNodeInfo
  { _rniHovered :: Bool
  , _rniFocusNode :: Maybe (FocusedNode a)
  , _rniFocusElement :: Maybe (Dom.Element Dom.EventResult GhcjsDomSpace t)
  }
makeLenses ''RenderNodeInfo
instance Reflex t => Semigroup (RenderNodeInfo t a) where
  rni1 <> rni2 =
    RenderNodeInfo
    { _rniHovered = (||) (_rniHovered rni1) (_rniHovered rni2)
    , _rniFocusNode = (<|>) (rni1 ^. rniFocusNode) (rni2 ^. rniFocusNode)
    , _rniFocusElement = (<|>) (rni1 ^. rniFocusElement) (rni2 ^. rniFocusElement)
    }
instance Reflex t => Monoid (RenderNodeInfo t a) where
  mempty =
    RenderNodeInfo
    { _rniHovered = False
    , _rniFocusNode = Nothing
    , _rniFocusElement = Nothing
    }

data RenderNodeEnv t a b
  = RenderNodeEnv
  { _rnContextMenuControls :: ContextMenuControls t
  , _rnNodeControls :: NodeControls t
  , _rnMenu :: Dynamic t Menu
  , _rnInitialErrors :: Trie b CheckError
  , _rnErrors :: Dynamic t (Trie b CheckError)
  , _rnInitialVersioned :: Versioned a
  , _rnVersioned :: Dynamic t (Versioned a)
  , _rnFocus :: Dynamic t (Focus b)
  , _rnPath :: Path a b
  }
makeClassy ''RenderNodeEnv

scribeDyn ::
  forall time s m t a b.
  (Reflex time, DynamicWriter time t m, Monoid s) =>
  ASetter s t a b ->
  Dynamic time b ->
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
  , EventWriter t (NodeEvent a) m

  , Show a
  ) =>
  Hash b ->
  Dynamic t (Hash b) ->
  m ()
renderNodeHash initialHash dHash = do
  initialVersioned <- view rnInitialVersioned
  dVersioned <- view rnVersioned
  let
    getNode h versioned =
      let
        Identity (mNode, _) = runVersionedT versioned $ Store.lookupNode h
      in
        (,) h <$> mNode
  renderMaybeNode
    (getNode initialHash initialVersioned)
    (traceDyn "node" $ getNode <$> dHash <*> dVersioned)

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
  let downErrors = Maybe.fromMaybe Trie.empty . Trie.down build
  env <- ask
  runReaderT m $
    RenderNodeEnv
    { _rnContextMenuControls = env ^. rnContextMenuControls
    , _rnNodeControls = env ^. rnNodeControls
    , _rnMenu = env ^. rnMenu
    , _rnInitialErrors = downErrors $ view rnInitialErrors env
    , _rnErrors = downErrors <$> view rnErrors env
    , _rnInitialVersioned = env ^. rnInitialVersioned
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

errorUnderline ::
  ( MonadHold t m
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , PostBuild t m
  , TriggerEvent t m
  , PerformEvent t m, MonadJSM (Performable m)
  , MonadJSM m
  ) =>
  Bool ->
  Event t Bool ->
  m a ->
  m ()
errorUnderline initialHasError eHasErrorUpdated m =
  networkHold_ (drawWhenHasError initialHasError) (drawWhenHasError <$> eHasErrorUpdated)
  where
    drawWhenHasError hasError =
      if hasError
      then do
        (element, _) <- Dom.elAttr' "span" [("class", "error-target")] m
        eAfterPostBuild <- delay 0.05 =<< getPostBuild
        networkHold_ (drawUnderline element) (drawUnderline element <$ eAfterPostBuild)
      else do
        _ <- m
        pure ()

    drawUnderline element = do
      rawElement :: HTMLElement <- GHCJS.DOM.unsafeCastTo HTMLElement (Dom._element_raw element)
      x <- ceiling <$> HTMLElement.getOffsetLeft rawElement
      y <- ceiling <$> HTMLElement.getOffsetTop rawElement
      width <- ceiling <$> HTMLElement.getOffsetWidth rawElement
      height <- ceiling <$> HTMLElement.getOffsetHeight rawElement
      svgUnderline (x, y + height) width

renderMaybeNode ::
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
  , EventWriter t (NodeEvent a) m

  , Show a
  ) =>
  Maybe (Hash b, Node b) ->
  Dynamic t (Maybe (Hash b, Node b)) ->
  m ()
renderMaybeNode initialmNode dmNode = do
  initialErrors <- view rnInitialErrors
  dErrors <- view rnErrors
  let
    mkHasError = maybe False (const True) . Trie.current
    initialHasError = mkHasError initialErrors
    eHasErrorUpdated = mkHasError <$> updated dErrors

  dHasError <- holdUniqDyn $ mkHasError <$> dErrors

  dInFocus <-
    holdUniqDyn =<<
    views rnFocus (fmap (\case; Focus Nil -> True; _ -> False))

  rec
    let
      dAttrs =
        (\mNode hovered hasError inFocus ->
            (if Maybe.maybe False isHole (snd <$> mNode)
              then "class" =: "syntax-hole"
              else mempty
            ) <>
            (case nodeType @b of
              TExpr -> "class" =: "syntax-expr"
              TBlock -> "class" =: "syntax-block"
              TStatement -> "class" =: "syntax-statement"
              TIdent -> "class" =: "syntax-ident"
              TArgs -> "class" =: "syntax-args"
              TParams -> "class" =: "syntax-params"
              TExprs -> "class" =: "syntax-exprs"
            ) <>
            (if hovered then "class" =: "syntax-hovered" else mempty) <>
            (if hasError then "class" =: "has-error" else mempty) <>
            (if inFocus then "class" =: "syntax-focused" else mempty)
        ) <$>
        dmNode <*>
        dElementHovered <*>
        dHasError <*>
        dInFocus

    let
      rendermNodeFactored ::
        Maybe ((Hash b, Node b), Dynamic t (Hash b, Node b)) ->
        m (Dynamic t Bool, Dom.Element Dom.EventResult GhcjsDomSpace t)
      rendermNodeFactored mdNode = do
        (element, dRenderNodeInfo) <-
          runDynamicWriterT . fmap fst . syntaxNodeD' dAttrs . errorUnderline initialHasError eHasErrorUpdated $
          case mdNode of
            Nothing ->
              Dom.text "error: missing node"
            Just (initialNode, dNode) -> do
              path <- view rnPath
              scribeDyn @t @(RenderNodeInfo t a) rniFocusNode $
                (\inFocus (hash, node) ->
                  if inFocus
                  then Just $ FocusedNode path hash node
                  else Nothing
                ) <$>
                dInFocus <*>
                dNode

              (initialNodeD, dNodeD) <- nodeDyn (snd initialNode) (snd <$> traceEvent "eNode" (updated dNode))

              networkHold_ (renderNodeD initialNodeD) (renderNodeD <$> traceEventWith (\case; NDefD{} -> "yes"; _ -> "no") (updated dNodeD))
        let
          eMouseenter = Dom.domEvent Dom.Mouseenter element
          eMouseleave = Dom.domEvent Dom.Mouseleave element
          eMousedown = Dom.domEvent Dom.Mousedown element

        dMouseInside <-
          case nodeType @b of
            TBlock ->
              pure $ view rniHovered <$> dRenderNodeInfo
            _ ->
              holdDyn False $ leftmost [True <$ eMouseenter, False <$ eMouseleave]

        dHovered <-
          case nodeType @b of
            TBlock -> pure $ constDyn False
            _ ->
              holdUniqDyn $
              (\inFocus inside childInfo ->
                inside && not (childInfo ^. rniHovered) && not inFocus
              ) <$>
              dInFocus <*>
              dMouseInside <*>
              dRenderNodeInfo

        let eClicked = gate (current dHovered) eMousedown

        dMenu <- view rnMenu
        controls <- view rnNodeControls
        path <- view rnPath
        let
          eOpenMenu =
            attachWithMaybe
              (\(inFocus, menu) () -> if inFocus && menu == MenuClosed then Just OpenMenu else Nothing)
              ((,) <$> current dInFocus <*> current dMenu)
              (ncOpenMenu controls)

          eCloseMenu =
            attachWithMaybe
              (\menu () -> if menu == MenuOpen then Just CloseMenu else Nothing)
              (current dMenu)
              (ncCloseMenu controls)

          eSelect =
            Select path <$ eClicked

        tellDyn dRenderNodeInfo

        -- why do I get an overlapping instance error without the type applications?
        scribeDyn @t @(RenderNodeInfo t a) rniHovered dMouseInside
        scribeDyn @t @(RenderNodeInfo t a) rniFocusElement $
          (\inFocus -> if inFocus then Just element else Nothing) <$>
          dInFocus

        tellEvent eOpenMenu
        tellEvent eCloseMenu
        tellEvent eSelect

        pure (dHovered, element)

    (initialmNodeFactored, dmNodeFactored) <- maybeDyn initialmNode dmNode
    (dElementHovered :: Dynamic t Bool, _dElement) <-
      fmap (\d -> (d >>= fst, snd <$> d)) $
      Reflex.Network.networkHold
        (rendermNodeFactored initialmNodeFactored)
        (rendermNodeFactored <$> updated dmNodeFactored)
  pure ()

renderNodeD ::
  forall t m r a b.
  ( DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , MonadHold t m
  , PerformEvent t m, MonadJSM (Performable m)
  , TriggerEvent t m
  , Adjustable t m
  , NotReady t m
  , PostBuild t m
  , HasRenderNodeEnv r t a b
  , MonadReader r m
  , MonadFix m
  , MonadJSM m
  , DynamicWriter t (RenderNodeInfo t a) m
  , EventWriter t (NodeEvent a) m

  , Show a
  ) =>
  NodeD t b ->
  m ()
renderNodeD node =
  case node of
    NEIdentD n ->
      Dom.dyn_ $ Dom.text . Text.pack <$> n
    NIdentD n ->
      Dom.dyn_ $ Dom.text . Text.pack <$> n
    NIHoleD ->
      syntaxHole mempty
    NForD initialIdent initialVal initialBody ident val body -> do
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "for"
        down
          (\case; For_Ident -> Just (Refl, Refl); _ -> Nothing)
          For_Ident
          (renderNodeHash initialIdent ident)
        syntaxKeyword mempty $ Dom.text "in"
        down
          (\case; For_Expr -> Just (Refl, Refl); _ -> Nothing)
          For_Expr
          (renderNodeHash initialVal val)
        syntaxColon mempty
      syntaxNested mempty $
        down
          (\case; For_Block -> Just (Refl, Refl); _ -> Nothing)
          For_Block
          (renderNodeHash initialBody body)
    NIfThenD initialCond initialThen cond then_ -> do
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "if"
        down
          (\case; IfThen_Cond -> Just (Refl, Refl); _ -> Nothing)
          IfThen_Cond
          (renderNodeHash initialCond cond)
        syntaxColon mempty
      syntaxNested mempty $
        down
          (\case; IfThen_Then -> Just (Refl, Refl); _ -> Nothing)
          IfThen_Then
          (renderNodeHash initialThen then_)
    NIfThenElseD initialCond initialThen initialElse cond then_ else_ -> do
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "if"
        down
          (\case; IfThenElse_Cond -> Just (Refl, Refl); _ -> Nothing)
          IfThenElse_Cond
          (renderNodeHash initialCond cond)
        syntaxColon mempty
      syntaxNested mempty $
        down
          (\case; IfThenElse_Then -> Just (Refl, Refl); _ -> Nothing)
          IfThenElse_Then
          (renderNodeHash initialThen then_)
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "else"
        syntaxColon mempty
      syntaxNested mempty $
        down
          (\case; IfThenElse_Else -> Just (Refl, Refl); _ -> Nothing)
          IfThenElse_Else
          (renderNodeHash initialElse else_)
    NPrintD initialVal val ->
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "print"
        syntaxColon mempty
        down
          (\case; Print_Value -> Just (Refl, Refl); _ -> Nothing)
          Print_Value
          (renderNodeHash initialVal val)
    NReturnD initialVal val ->
      syntaxLine mempty $ do
        syntaxKeyword mempty $ Dom.text "return"
        syntaxColon mempty
        down
          (\case; Return_Value -> Just (Refl, Refl); _ -> Nothing)
          Return_Value
          (renderNodeHash initialVal val)
    NDefD initialName initialArgs initialBody name args body -> do
      syntaxLine mempty $ do
        liftIO $ putStrLn "test"
        syntaxKeyword mempty $ Dom.text "def"
        down
          (\case; Def_Name -> Just (Refl, Refl); _ -> Nothing)
          Def_Name
          (renderNodeHash initialName name)
        syntaxLParen mempty
        down
          (\case; Def_Args -> Just (Refl, Refl); _ -> Nothing)
          Def_Args
          (renderNodeHash initialArgs args)
        syntaxRParen mempty
        syntaxColon mempty
      syntaxNested mempty $
        down
          (\case; Def_Body -> Just (Refl, Refl); _ -> Nothing)
          Def_Body
          (renderNodeHash initialBody body)
    NBoolD initialB dB ->
      let
        render b = syntaxLiteral mempty . Dom.text $ if b then "true" else "false"
      in
        networkHold_ (render initialB) (render <$> updated dB)
    NIntD initialN n ->
      let
        render = syntaxLiteral mempty . Dom.text . Text.pack . show
      in
        networkHold_ (render initialN) (render <$> updated n)
    NBinOpD initialOp initialLeft initialRight dOp left right ->
      let
        renderOp =
          \case
            Add -> syntaxSymbol mempty $ Dom.text "+"
            Sub -> syntaxSymbol mempty $ Dom.text "-"
            Mul -> syntaxSymbol mempty $ Dom.text "*"
            Div -> syntaxSymbol mempty $ Dom.text "/"
            Eq -> syntaxSymbol mempty $ Dom.text "=="
            And -> syntaxKeyword mempty $ Dom.text "and"
            Or -> syntaxKeyword mempty $ Dom.text "or"
      in
        syntaxInline mempty $ do
          down
            (\case; BinOp_Left -> Just (Refl, Refl); _ -> Nothing)
            BinOp_Left
            (renderNodeHash initialLeft left)
          networkHold_ (renderOp initialOp) (renderOp <$> updated dOp)
          down
            (\case; BinOp_Right -> Just (Refl, Refl); _ -> Nothing)
            BinOp_Right
            (renderNodeHash initialRight right)
    NUnOpD initialOp initialVal dOp val ->
      let
        renderOp =
          \case
            Neg -> syntaxSymbol mempty $ Dom.text "-"
            Not -> syntaxKeyword mempty $ Dom.text "not"
      in
        syntaxInline mempty $ do
          networkHold_ (renderOp initialOp) (renderOp <$> updated dOp)
          down
            (\case; UnOp_Value -> Just (Refl, Refl); _ -> Nothing)
            UnOp_Value
            (renderNodeHash initialVal val)
    NCallD initialFunc initialArgs func args ->
      syntaxInline mempty $ do
        down
          (\case; Call_Function -> Just (Refl, Refl); _ -> Nothing)
          Call_Function
          (renderNodeHash initialFunc func)
        syntaxLParen mempty
        down
          (\case; Call_Args -> Just (Refl, Refl); _ -> Nothing)
          Call_Args
          (renderNodeHash initialArgs args)
        syntaxRParen mempty
    NListD initialExprs exprs -> do
      syntaxInline mempty $ do
        syntaxLBracket mempty
        down
          (\case; List_Exprs -> Just (Refl, Refl); _ -> Nothing)
          List_Exprs
          (renderNodeHash initialExprs exprs)
        syntaxRBracket mempty
    NExprsD initialXs xs ->
      let
        render =
          itraverse_
             (\ix (initialX, x) ->
               down
                 (\case; Exprs_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                 (Exprs_Index ix)
                 (renderNodeHash initialX x)
             )
      in
        syntaxInline mempty $ do
          (initialldX, dldX) <- listDyn initialXs xs
          networkHold_ (render $ initialldX) (render <$> updated dldX)
    NBlockD initialSts sts -> do
      let
        render =
          itraverse_
            (\ix (initialX, x) ->
               down
                 (\case; Block_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                 (Block_Index ix)
                 (renderNodeHash initialX x)
            )
      (initialldSt, dldSt) <- listDyn (NonEmpty.toList initialSts) (NonEmpty.toList <$> sts)
      networkHold_ (render $ initialldSt) (render <$> updated dldSt)
    NArgsD initialXs xs -> do
      let
        render =
          traverse_
            (\item ->
              case item of
                Nothing ->
                  syntaxComma mempty
                Just (ix, (initialX, x)) ->
                  down
                    (\case; Args_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                    (Args_Index ix)
                    (renderNodeHash initialX x)
            ) .
            List.intersperse Nothing . fmap Just . zip [0::Int ..]
      syntaxInline mempty $ do
        (initialldX, dldX) <- listDyn initialXs xs
        networkHold_ (render $ initialldX) (render <$> updated dldX)
    NParamsD initialXs xs -> do
      let
        render =
          traverse_
            (\item ->
              case item of
                Nothing ->
                  syntaxComma mempty
                Just (ix, (initialX, x)) ->
                  down
                    (\case; Params_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                    (Params_Index ix)
                    (renderNodeHash initialX x)
            ) .
            List.intersperse Nothing . fmap Just . zip [0::Int ..]
      syntaxInline mempty $ do
        (initialldX, dldX) <- listDyn initialXs xs
        networkHold_ (render $ initialldX) (render <$> updated dldX)
    NSHoleD ->
      syntaxHole mempty
    NEHoleD ->
      syntaxHole mempty
