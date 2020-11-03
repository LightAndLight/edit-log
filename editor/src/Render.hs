{-# language FlexibleContexts #-}
{-# language FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# language GADTs, KindSignatures #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables, TypeApplications #-}
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

import Control.Applicative ((<|>))
import Control.Lens.Indexed (itraverse_)
import Control.Lens.Getter ((^.), view, views)
import Control.Lens.Setter (ASetter, (.~))
import Control.Lens.TH (makeClassy, makeLenses)
import Control.Monad (join)
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
import Reflex
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
import Node.Dynamic (NodeD(..), nodeDyn)
import Svg (svgElAttr)

networkHold_ :: (Adjustable t m, MonadHold t m) => m a -> Event t (m a) -> m ()
networkHold_ child0 newChild = do
  (_, _) <- runWithReplace child0 newChild
  pure ()

sequenceDyn_ :: (MonadHold t m, Adjustable t m) => Dynamic t (m a) -> m ()
sequenceDyn_ d = networkHold_ (join $ sample (current d)) (updated d)

forDyn_ ::
  (MonadHold t m, Adjustable t m) =>
  Dynamic t a ->
  (a -> m b) ->
  m ()
forDyn_ d f = networkHold_ (f =<< sample (current d)) (f <$> updated d)

forDyn ::
  (MonadHold t m, Adjustable t m) =>
  Dynamic t a ->
  (a -> m b) ->
  m (Dynamic t b)
forDyn d f = Reflex.Network.networkHold (f =<< sample (current d)) (f <$> updated d)

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

instance Semigroup (NodeEvent a) where
  l <> _ = l

data FocusedNode a where
  FocusedNode :: KnownNodeType b => Path a b -> Hash b -> Node b -> FocusedNode a

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
  , _rnErrors :: Dynamic t (Trie b CheckError)
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
  ) =>
  Dynamic t (Hash b) ->
  m ()
renderNodeHash dHash = do
  dHasError <-
    holdUniqDyn =<<
    views rnErrors (fmap $ maybe False (const True) . Trie.current)

  dInFocus <-
    holdUniqDyn =<<
    views rnFocus (fmap (\case; Focus Nil -> True; _ -> False))

  rec
    let
      eMouseenter = switchDyn $ Dom.domEvent Dom.Mouseenter <$> dNodeElement
      eMouseleave = switchDyn $ Dom.domEvent Dom.Mouseleave <$> dNodeElement
      eMousedown = switchDyn $ Dom.domEvent Dom.Mousedown <$> dNodeElement


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

    dVersioned <- view rnVersioned
    let
      dmNode =
        (\h versioned ->
           let
             Identity (mNode, _) = runVersionedT versioned $ Store.lookupNode h
           in
              (,) h <$> mNode
        ) <$>
        dHash <*>
        dVersioned
    (dNodeElement, dRenderNodeInfo) <-
      runDynamicWriterT $
      renderNode dInFocus dHasError dHovered dmNode

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

  tellEvent eOpenMenu
  tellEvent eCloseMenu
  tellEvent eSelect

  -- why do I get an overlapping instance error without the type applications?
  scribeDyn @t @(RenderNodeInfo t a) rniHovered dMouseInside
  scribeDyn @t @(RenderNodeInfo t a) rniFocusElement $
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

errorUnderline ::
  ( MonadHold t m
  , DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace
  , PostBuild t m
  , TriggerEvent t m
  , PerformEvent t m, MonadJSM (Performable m)
  , MonadJSM m
  ) =>
  Dynamic t Bool ->
  m a ->
  m ()
errorUnderline dHasError m =
  forDyn_ dHasError drawWhenHasError
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
  , EventWriter t (NodeEvent a) m
  ) =>
  Dynamic t Bool ->
  Dynamic t Bool ->
  Dynamic t Bool ->
  Dynamic t (Maybe (Hash b, Node b)) ->
  m (Dynamic t (Dom.Element Dom.EventResult GhcjsDomSpace t))
renderNode dInFocus dHasError dHovered dmNode = do
  path <- view rnPath
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
      dHovered <*>
      dHasError <*>
      dInFocus

  dmNodeFactored <- maybeDyn dmNode
  forDyn dmNodeFactored $
    \mdNode ->
    fmap fst . syntaxNodeD' dAttrs . errorUnderline dHasError $
      case mdNode of
        Nothing ->
          Dom.text "error: missing node"
        Just dNode -> do
          scribeDyn @t @(RenderNodeInfo t a) rniFocusNode $
            (\inFocus (hash, node) ->
              if inFocus
              then Just $ FocusedNode path hash node
              else Nothing
            ) <$>
            dInFocus <*>
            dNode

          dNodeD <- nodeDyn (snd <$> dNode)

          forDyn_ dNodeD nodeDom

  where
    nodeDom :: NodeD t b -> m ()
    nodeDom node =
      case node of
        NEIdentD n ->
          Dom.dyn_ $ Dom.text . Text.pack <$> n
        NIdentD n ->
          Dom.dyn_ $ Dom.text . Text.pack <$> n
        NIHoleD ->
          syntaxHole mempty
        NForD ident val body -> do
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
        NIfThenD cond then_ -> do
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
        NIfThenElseD cond then_ else_ -> do
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
        NPrintD val ->
          syntaxLine mempty $ do
            syntaxKeyword mempty $ Dom.text "print"
            syntaxColon mempty
            down
              (\case; Print_Value -> Just (Refl, Refl); _ -> Nothing)
              Print_Value
              (renderNodeHash val)
        NReturnD val ->
          syntaxLine mempty $ do
            syntaxKeyword mempty $ Dom.text "return"
            syntaxColon mempty
            down
              (\case; Return_Value -> Just (Refl, Refl); _ -> Nothing)
              Return_Value
              (renderNodeHash val)
        NDefD name args body -> do
          syntaxLine mempty $ do
            syntaxKeyword mempty $ Dom.text "def"
            down
              (\case; Def_Name -> Just (Refl, Refl); _ -> Nothing)
              Def_Name
              (renderNodeHash name)
            syntaxLParen mempty
            down
              (\case; Def_Args -> Just (Refl, Refl); _ -> Nothing)
              Def_Args
              (renderNodeHash args)
            syntaxRParen mempty
            syntaxColon mempty
          syntaxNested mempty $
            down
              (\case; Def_Body -> Just (Refl, Refl); _ -> Nothing)
              Def_Body
              (renderNodeHash body)
        NBoolD dB ->
          sequenceDyn_ $
          syntaxLiteral mempty . Dom.text .
          (\b -> if b then "true" else "false") <$> dB
        NIntD n ->
          sequenceDyn_ $
          syntaxLiteral mempty . Dom.text .
          Text.pack . show <$> n
        NBinOpD dOp left right ->
          syntaxInline mempty $ do
            down
              (\case; BinOp_Left -> Just (Refl, Refl); _ -> Nothing)
              BinOp_Left
              (renderNodeHash left)
            sequenceDyn_ $
              (\case
                Add -> syntaxSymbol mempty $ Dom.text "+"
                Sub -> syntaxSymbol mempty $ Dom.text "-"
                Mul -> syntaxSymbol mempty $ Dom.text "*"
                Div -> syntaxSymbol mempty $ Dom.text "/"
                Eq -> syntaxSymbol mempty $ Dom.text "=="
                And -> syntaxKeyword mempty $ Dom.text "and"
                Or -> syntaxKeyword mempty $ Dom.text "or"
              ) <$>
              dOp
            down
              (\case; BinOp_Right -> Just (Refl, Refl); _ -> Nothing)
              BinOp_Right
              (renderNodeHash right)
        NUnOpD dOp val ->
          syntaxInline mempty $ do
            sequenceDyn_ $
              (\case
                Neg -> syntaxSymbol mempty $ Dom.text "-"
                Not -> syntaxKeyword mempty $ Dom.text "not"
              ) <$>
              dOp
            down
              (\case; UnOp_Value -> Just (Refl, Refl); _ -> Nothing)
              UnOp_Value
              (renderNodeHash val)
        NCallD func args ->
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
        NListD exprs -> do
          syntaxInline mempty $ do
            syntaxLBracket mempty
            down
              (\case; List_Exprs -> Just (Refl, Refl); _ -> Nothing)
              List_Exprs
              (renderNodeHash exprs)
            syntaxRBracket mempty
        NExprsD xs ->
          syntaxInline mempty $ do
            dldX <- listDyn xs
            forDyn_ dldX
              (itraverse_ $
               \ix x ->
                 down
                   (\case; Exprs_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                   (Exprs_Index ix)
                   (renderNodeHash x)
              )
        NBlockD sts -> do
          dldSt <- listDyn $ NonEmpty.toList <$> sts
          forDyn_ dldSt
            (itraverse_ $
              \ix x ->
                down
                  (\case; Block_Index ix' | ix == ix' -> Just (Refl, Refl); _ -> Nothing)
                  (Block_Index ix)
                  (renderNodeHash x)
            )
        NArgsD xs -> do
          syntaxInline mempty $ do
            dldX <- listDyn xs
            forDyn_ dldX $
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
                ) .
                List.intersperse Nothing . fmap Just . zip [0::Int ..]
        NParamsD xs -> do
          syntaxInline mempty $ do
            dldX <- listDyn xs
            forDyn_ dldX $
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
                ) .
                List.intersperse Nothing . fmap Just . zip [0::Int ..]
        NSHoleD ->
          syntaxHole mempty
        NEHoleD ->
          syntaxHole mempty
