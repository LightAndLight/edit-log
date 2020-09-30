{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Render where

import Data.Foldable (sequence_, traverse_, for_)
import Data.Function (on)
import Data.Functor.Identity (runIdentity)
import qualified Data.List as List
import Data.Witherable (wither)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

import Diff (Diff)
import qualified Diff as Diff
import Node (Hash)
import Path (Level(..))
import Store (MonadStore)
import qualified Store
import Syntax (Ident(..), Statement(..), Expr(..), Block(..), BinOp(..), UnOp(..))

renderLeafChange ::
  MonadStore m =>
  Html.AttributeValue ->
  Diff.LeafChange a ->
  (a -> Html) ->
  m Html
renderLeafChange nodeType change render =
  case change of
    Diff.ReplaceLeaf oldh newh -> do
      m_old <- Store.rebuild oldh
      case m_old of
        Nothing -> error $ "failed to rebuild " <> show oldh
        Just old -> do
          m_new <- Store.rebuild newh
          case m_new of
            Nothing -> error $ "failed to rebuild " <> show newh
            Just new ->
              pure $
              Html.div ! Attr.class_ ("syntax-branch " <> nodeType <> " diff") $ do
                Html.div ! Attr.class_ "diff-removed" $ do
                  render old
                Html.div ! Attr.class_ "diff-added" $ do
                  render new
    Diff.InsertLeaf (Diff.InsertPositions positions) -> do
      let
        positions' :: [(Int, Hash Statement)]
        positions' =
          List.sortBy (compare `on` fst ) positions >>=
          \(ix, vals) -> fmap (\(pos, val) -> (ix+pos, val)) (zip [0..] vals)

      positions'' :: [(Int, Statement)] <-
        wither
          (\(ix, valh) -> do
             m_val <- Store.rebuild valh
             pure $ (,) ix <$> m_val
          )
          positions'
      pure $
        Html.div ! Attr.class_ ("syntax-branch " <> nodeType <> " diff") $ do
          Html.div ! Attr.class_ "diff-added" $ do
            for_ positions'' $ \(ix, val) -> do
              Html.div ! Attr.class_ "diff-annotation" $ Html.toHtml (show ix)
              renderStatement val

renderBlockWithDiff :: MonadStore m => Block -> Diff Block -> m Html
renderBlockWithDiff block diff =
  case diff of
    Diff.Leaf change -> renderLeafChange "block" change renderBlock
    Diff.Branch branchChanges entries ->
      case block of
        BHole ->
          case entries of
            [] -> pure renderBHole
            _:_ -> error "there's no Level that can walk into a BHole"
        Block sts -> do
          htmls <-
            traverse
              (\(ix, st) ->
                 case Diff.getEntry (Block_Index ix) entries of
                   Nothing -> pure $ renderStatement st
                   Just diff' -> renderStatementWithDiff st diff'
              )
              (zip [0::Int ..] sts)
          pure $ sequence_ htmls

withDiff ::
  Applicative m =>
  (b -> Html) ->
  (b -> Diff b -> m Html) ->
  Level a b ->
  [Diff.Entry a] ->
  b ->
  m Html
withDiff render renderWithDiff level entries val =
  case Diff.getEntry level entries of
    Nothing -> pure $ render val
    Just diff' -> renderWithDiff val diff'

renderStatementWithDiff :: MonadStore m => Statement -> Diff Statement -> m Html
renderStatementWithDiff statement diff =
  case diff of
    Diff.Leaf change -> renderLeafChange "statement" change renderStatement
    Diff.Branch branchChanges entries ->
      case statement of
        For ident expr body ->
          renderStatement_For
            (withDiff renderIdent renderIdentWithDiff For_Ident entries)
            (withDiff renderExpr renderExprWithDiff For_Expr entries)
            (withDiff renderBlock renderBlockWithDiff For_Block entries)
            ident
            expr
            body
        IfThen cond then_ ->
          renderStatement_IfThen
            (withDiff renderExpr renderExprWithDiff IfThen_Cond entries)
            (withDiff renderBlock renderBlockWithDiff IfThen_Then entries)
            cond
            then_
        IfThenElse cond then_ else_ ->
          renderStatement_IfThenElse
            (withDiff renderExpr renderExprWithDiff IfThenElse_Cond entries)
            (withDiff renderBlock renderBlockWithDiff IfThenElse_Then entries)
            (withDiff renderBlock renderBlockWithDiff IfThenElse_Else entries)
            cond
            then_
            else_
        SHole ->
          case entries of
            [] -> pure renderSHole
            _:_ -> error "there's no Level that can walk into a SHole"

renderExprWithDiff :: MonadStore m => Expr -> Diff Expr -> m Html
renderExprWithDiff expr diff =
  case diff of
    Diff.Leaf change -> renderLeafChange "expr" change renderExpr
    Diff.Branch branchChanges entries ->
      case expr of
        Bool b ->
          case entries of
            [] -> pure $ renderExpr_Bool b
            _:_ -> error "there's no Level that can walk into a Bool"
        Int n ->
          case entries of
            [] -> pure $ renderExpr_Int n
            _:_ -> error "there's no Level that can walk into an Int"
        BinOp op left right ->
          renderExpr_BinOp
            (withDiff renderExpr renderExprWithDiff BinOp_Left entries)
            (withDiff renderExpr renderExprWithDiff BinOp_Right entries)
            op
            left
            right
        UnOp op value ->
          renderExpr_UnOp
            (withDiff renderExpr renderExprWithDiff UnOp_Value entries)
            op
            value
        EHole ->
          case entries of
            [] -> pure renderEHole
            _:_ -> error "there's no Level that can walk into an EHole"

renderIdentWithDiff :: MonadStore m => Ident -> Diff Ident -> m Html
renderIdentWithDiff ident diff =
  case diff of
    Diff.Leaf change -> renderLeafChange "ident" change renderIdent
    Diff.Branch branchChanges entries ->
      case entries of
        [] -> pure $ renderIdent ident
        _:_ -> error "there's no Level that can walk into an Ident"

renderIdent :: Ident -> Html
renderIdent (Ident ident) =
  Html.div ! Attr.class_ "syntax-leaf identifier" $ Html.toHtml ident

renderSHole :: Html
renderSHole =
  Html.div ! Attr.class_ "syntax-leaf statement hole" $ do
    pure ()

renderStatement_For ::
  Monad m =>
  (Ident -> m Html) ->
  (Expr -> m Html) ->
  (Block -> m Html) ->
  Ident -> Expr -> Block -> m Html
renderStatement_For fIdent fExpr fBody ident cond body = do
  identHtml <- fIdent ident
  exprHtml <- fExpr cond
  bodyHtml <- fBody body
  pure $
    Html.div ! Attr.class_ "syntax-branch statement for" $ do
      Html.div ! Attr.class_ "syntax-line" $ do
        Html.div ! Attr.class_ "syntax-keyword" $ "for"
        identHtml
        Html.div ! Attr.class_ "syntax-keyword" $ "in"
        exprHtml
        Html.div ! Attr.class_ "syntax-symbol" $ ":"
      bodyHtml

renderStatement_IfThen ::
  Monad m =>
  (Expr -> m Html) ->
  (Block -> m Html) ->
  Expr -> Block -> m Html
renderStatement_IfThen fCond fThen cond then_ = do
  condHtml <- fCond cond
  thenHtml <- fThen then_
  pure $
    Html.div ! Attr.class_ "syntax-branch statement if-then" $ do
      Html.div ! Attr.class_ "syntax-line" $ do
        Html.div ! Attr.class_ "syntax-keyword" $ "if"
        condHtml
        Html.div ! Attr.class_ "syntax-symbol" $ ":"
      thenHtml

renderStatement_IfThenElse ::
  Monad m =>
  (Expr -> m Html) ->
  (Block -> m Html) ->
  (Block -> m Html) ->
  Expr -> Block -> Block -> m Html
renderStatement_IfThenElse fCond fThen fElse cond then_ else_ = do
  condHtml <- fCond cond
  thenHtml <- fThen then_
  elseHtml <- fElse else_
  pure $
    Html.div ! Attr.class_ "syntax-branch statement if-then-else" $ do
      Html.div ! Attr.class_ "syntax-line" $ do
        Html.div ! Attr.class_ "syntax-keyword" $ "if"
        condHtml
        Html.div ! Attr.class_ "syntax-symbol" $ ":"
      thenHtml
      Html.div ! Attr.class_ "syntax-line" $ do
        Html.div ! Attr.class_ "syntax-keyword" $ "else"
        Html.div ! Attr.class_ "syntax-symbol" $ ":"
      elseHtml

renderStatement :: Statement -> Html
renderStatement statement =
  case statement of
    For ident expr body ->
      runIdentity $
      renderStatement_For
        (pure . renderIdent)
        (pure . renderExpr)
        (pure . renderBlock)
        ident
        expr
        body
    IfThen cond then_ ->
      runIdentity $
      renderStatement_IfThen
        (pure . renderExpr)
        (pure . renderBlock)
        cond
        then_
    IfThenElse cond then_ else_ ->
      runIdentity $
      renderStatement_IfThenElse
        (pure . renderExpr)
        (pure . renderBlock)
        (pure . renderBlock)
        cond
        then_
        else_
    SHole -> renderSHole

renderBinOp :: BinOp -> Html
renderBinOp op =
  Html.div ! Attr.class_ "syntax-leaf" $
  case op of
    Add -> Html.div ! Attr.class_ "syntax-symbol" $ "+"
    Sub -> Html.div ! Attr.class_ "syntax-symbol" $ "-"
    Mul -> Html.div ! Attr.class_ "syntax-symbol" $ "*"
    Div -> Html.div ! Attr.class_ "syntax-symbol" $ "/"
    Or -> Html.div ! Attr.class_ "syntax-keyword" $ "or"
    And -> Html.div ! Attr.class_ "syntax-keyword" $ "and"

renderUnOp :: UnOp -> Html
renderUnOp op =
  Html.div ! Attr.class_ "syntax-leaf" $
  case op of
    Neg -> Html.div ! Attr.class_ "syntax-symbol" $ "-"
    Not -> Html.div ! Attr.class_ "syntax-keyword" $ "not"

renderEHole :: Html
renderEHole =
  Html.div ! Attr.class_ "syntax-leaf expression hole" $ do
    pure ()

renderExpr_Bool :: Bool -> Html
renderExpr_Bool b =
  Html.div ! Attr.class_ "syntax-leaf expression bool" $ do
    Html.div ! Attr.class_ "syntax-keyword" $ do
      case b of
        True -> "true"
        False -> "false"

renderExpr_Int :: Int -> Html
renderExpr_Int n =
  Html.div ! Attr.class_ "syntax-leaf expression int" $
    Html.toHtml (show n)

renderExpr_BinOp ::
  Monad m =>
  (Expr -> m Html) ->
  (Expr -> m Html) ->
  BinOp -> Expr -> Expr -> m Html
renderExpr_BinOp fLeft fRight op left right = do
  leftHtml <- fLeft left
  rightHtml <- fRight right
  pure $
    Html.div ! Attr.class_ "syntax-branch expression binop" $ do
      Html.div ! Attr.class_ "syntax-line" $ do
        leftHtml
        renderBinOp op
        rightHtml

renderExpr_UnOp ::
  Monad m =>
  (Expr -> m Html) ->
  UnOp -> Expr -> m Html
renderExpr_UnOp fValue op value = do
  valueHtml <- fValue value
  pure $
    Html.div ! Attr.class_ "syntax-branch expression unop" $ do
      Html.div ! Attr.class_ "syntax-line" $ do
        renderUnOp op
        valueHtml

renderExpr :: Expr -> Html
renderExpr expr =
  case expr of
    Bool b -> renderExpr_Bool b
    Int n -> renderExpr_Int n
    BinOp op left right ->
      runIdentity $
      renderExpr_BinOp
        (pure . renderExpr)
        (pure . renderExpr)
        op
        left
        right
    UnOp op value ->
      runIdentity $
      renderExpr_UnOp
        (pure . renderExpr)
        op
        value
    EHole -> renderEHole

renderBHole :: Html
renderBHole =
  Html.div ! Attr.class_ "syntax-leaf block hole" $ do
    pure ()

renderBlock :: Block -> Html
renderBlock block =
  case block of
    BHole -> renderBHole
    Block sts ->
      Html.div ! Attr.class_ "syntax-branch block" $ do
        traverse_ renderStatement sts
