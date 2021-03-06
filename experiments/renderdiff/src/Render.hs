{-# language EmptyCase #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Render where

import Data.Foldable (sequence_, traverse_)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty(..))
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

import Diff (Diff)
import qualified Diff.SequenceDiff as SequenceDiff
import qualified Diff as Diff
import Hash (Hash)
import Path (Level(..))
import Store (MonadStore)
import qualified Store
import Syntax (Ident(..), Statement(..), Expr(..), Block(..), BinOp(..), UnOp(..))

renderStandaloneBlockChange :: (Int, SequenceDiff.Change Statement) -> Html
renderStandaloneBlockChange (_, change) =
  Html.div ! Attr.class_ ("diff statement") $
  case change of
    SequenceDiff.Insert vals ->
      Html.div ! Attr.class_ "diff-added diff-insert" $ do
        traverse_ renderStatement vals
    SequenceDiff.Replace{} ->
      error $ "replace is not a standlone change"
    SequenceDiff.Delete ->
      error $ "delete is not a standalone change"

renderOverlaidBlockChange ::
  Monad m =>
  ((Int, Statement) -> m Html) ->
  (Int, SequenceDiff.Change Statement) ->
  (Int, Statement) ->
  m Html
renderOverlaidBlockChange renderOldStatement (_, change) old = do
  oldHtml <- renderOldStatement old
  pure $
    case change of
      SequenceDiff.Insert vals -> do
        oldHtml
        Html.div ! Attr.class_ ("diff statement") $
          Html.div ! Attr.class_ "diff-added diff-insert" $
            traverse_ renderStatement vals
      SequenceDiff.Replace vals ->
        Html.div ! Attr.class_ ("diff statement") $ do
          Html.div ! Attr.class_ "diff-removed diff-top" $
            oldHtml
          Html.div ! Attr.class_ "diff-added diff-bottom" $ do
            traverse_ renderStatement vals
      SequenceDiff.Delete ->
        Html.div ! Attr.class_ ("diff statement") $ do
          Html.div ! Attr.class_ "diff-removed" $
            oldHtml

renderLeafChange ::
  MonadStore m =>
  Html.AttributeValue ->
  Diff.LeafChange a ->
  a ->
  (a -> Html) ->
  m Html
renderLeafChange nodeType change item render =
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
              Html.div ! Attr.class_ ("diff " <> nodeType) $ do
                Html.div ! Attr.class_ "diff-removed diff-top" $ do
                  render old
                Html.div ! Attr.class_ "diff-added diff-bottom" $ do
                  render new
    Diff.EditBlockLeaf changes | Block statements <- item -> do
      let
        changes' :: [(Int, SequenceDiff.Change (Hash Statement))]
        changes' = SequenceDiff.toList changes

      changes'' :: [(Int, SequenceDiff.Change Statement)] <-
        (traverse.traverse.traverse)
          (\valh -> do
             m_val <- Store.rebuild valh
             case m_val of
               Nothing -> error $ "failed to rebuild " <> show valh
               Just val -> pure val
          )
          changes'

      let
        statements' :: [(Int, Statement)]
        statements' = zip [0..] statements

      pure $ overlayBlockChanges changes'' statements'
  where
    overlayBlockChanges ::
      [(Int, SequenceDiff.Change Statement)] ->
      [(Int, Statement)] ->
      Html
    overlayBlockChanges news olds =
      case news of
        [] -> traverse_ (renderStatement . snd) olds
        new : newRest ->
          case olds of
            [] -> traverse_ renderStandaloneBlockChange news
            old : oldRest ->
              case compare (fst new) (fst old) of
                LT -> do -- render the new one
                  renderStandaloneBlockChange new
                  overlayBlockChanges newRest olds
                EQ -> do
                  runIdentity $ renderOverlaidBlockChange (pure . renderStatement . snd) new old
                  overlayBlockChanges newRest oldRest
                GT -> do -- render the old one
                  renderStatement $ snd old
                  overlayBlockChanges news oldRest

renderBlock_Statements :: Monad m => ([Statement] -> m Html) -> [Statement] -> m Html
renderBlock_Statements fSts sts = do
  stsHtml <- fSts sts
  pure $
    Html.div ! Attr.class_ "syntax-branch block" $ do
      stsHtml

renderBlock :: Block -> Html
renderBlock block =
  case block of
    Block sts ->
      runIdentity $
      renderBlock_Statements
        (pure . traverse_ renderStatement)
        sts

renderBlockWithDiff :: forall m. MonadStore m => Block -> Diff Block -> m Html
renderBlockWithDiff block diff =
  case diff of
    Diff.Empty -> pure $ renderBlock block
    Diff.Leaf change ->
      renderLeafChange "block" change block renderBlock
    Diff.Branch m_branchChange entries ->
      case block of
        Block sts_ -> do
          renderBlock_Statements
            (\sts ->
              case m_branchChange of
                Nothing -> do
                  htmls <-
                    traverse
                      (renderStatementIndexed entries)
                      (zip [0::Int ..] sts)
                  pure $ sequence_ htmls
                Just branchChange ->
                  case branchChange of
                    Diff.EditBlockBranch changes -> do
                      let
                        changes' :: [(Int, SequenceDiff.Change (Hash Statement))]
                        changes' = SequenceDiff.toList changes

                      changes'' :: [(Int, SequenceDiff.Change Statement)] <-
                        (traverse.traverse.traverse)
                          (\valh -> do
                            m_val <- Store.rebuild valh
                            case m_val of
                              Nothing -> error $ "failed to rebuild " <> show valh
                              Just val -> pure val
                          )
                          changes'

                      let
                        sts' :: [(Int, Statement)]
                        sts' = zip [0..] sts

                      blockHtml <- overlayBlockChanges (renderStatementIndexed entries) changes'' sts'

                      pure blockHtml
            )
            sts_
  where
    renderStatementIndexed :: Foldable f => f (Diff.Entry Block) -> (Int, Statement) -> m Html
    renderStatementIndexed entries (ix, st) =
      case Diff.getEntry (Block_Index ix) entries of
        Nothing -> pure $ renderStatement st
        Just diff' -> renderStatementWithDiff st diff'

    overlayBlockChanges ::
      ((Int, Statement) -> m Html) ->
      [(Int, SequenceDiff.Change Statement)] ->
      [(Int, Statement)] ->
      m Html
    overlayBlockChanges renderOldStatement news olds =
      case news of
        [] -> sequence_ <$> traverse renderOldStatement olds
        new : newRest ->
          case olds of
            [] -> pure $ traverse_ renderStandaloneBlockChange news
            old : oldRest ->
              case compare (fst new) (fst old) of
                LT -> do -- render the new one
                  restHtml <- overlayBlockChanges renderOldStatement newRest olds
                  pure $ do
                    renderStandaloneBlockChange new
                    restHtml
                EQ -> do
                  currentHtml <- renderOverlaidBlockChange renderOldStatement new old
                  restHtml <- overlayBlockChanges renderOldStatement newRest oldRest
                  pure $ do
                    currentHtml
                    restHtml
                GT -> do -- render the old one
                  oldHtml <- renderOldStatement old
                  restHtml <- overlayBlockChanges renderOldStatement news oldRest
                  pure $ do
                    oldHtml
                    restHtml

withDiff ::
  (Applicative m, Foldable f) =>
  (b -> Html) ->
  (b -> Diff b -> m Html) ->
  Level a b ->
  f (Diff.Entry a) ->
  b ->
  m Html
withDiff render renderWithDiff level entries val =
  case Diff.getEntry level entries of
    Nothing -> pure $ render val
    Just diff' -> renderWithDiff val diff'

renderStatementWithDiff :: MonadStore m => Statement -> Diff Statement -> m Html
renderStatementWithDiff statement diff =
  case diff of
    Diff.Empty -> pure $ renderStatement statement
    Diff.Leaf change -> renderLeafChange "statement" change statement renderStatement
    Diff.Branch m_branchChange entries ->
      case m_branchChange of
        Nothing ->
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
            Print val ->
              renderStatement_Print
                (withDiff renderExpr renderExprWithDiff Print_Value entries)
                val
            SHole ->
              case entries of
                _:|_ -> error "there's no Level that can walk into a SHole"
        Just branchChange ->
          case branchChange of

renderExprWithDiff :: MonadStore m => Expr -> Diff Expr -> m Html
renderExprWithDiff expr diff =
  case diff of
    Diff.Empty -> pure $ renderExpr expr
    Diff.Leaf change -> renderLeafChange "expr" change expr renderExpr
    Diff.Branch m_branchChange entries ->
      case m_branchChange of
        Nothing ->
          case expr of
            Bool{} ->
              case entries of
                _:|_ -> error "there's no Level that can walk into a Bool"
            Int{} ->
              case entries of
                _:|_ -> error "there's no Level that can walk into an Int"
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
                _:|_ -> error "there's no Level that can walk into an EHole"
        Just branchChange ->
          case branchChange of

renderIdentWithDiff :: MonadStore m => Ident -> Diff Ident -> m Html
renderIdentWithDiff ident diff =
  case diff of
    Diff.Empty -> pure $ renderIdent ident
    Diff.Leaf change -> renderLeafChange "ident" change ident renderIdent
    Diff.Branch m_branchChange entries ->
      case m_branchChange of
        Nothing ->
          case entries of
            _:|_ -> error "there's no Level that can walk into an Ident"
        Just branchChange ->
          case branchChange of

renderIdent :: Ident -> Html
renderIdent (Ident ident) =
  Html.div ! Attr.class_ "syntax-leaf identifier" $ Html.toHtml ident

renderSHole :: Html
renderSHole =
  Html.div ! Attr.class_ "syntax-leaf statement hole" $
  Html.div ! Attr.class_ "syntax-line" $
  Html.div ! Attr.class_ "syntax-symbol" $
  "_"

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

renderStatement_Print ::
  Monad m =>
  (Expr -> m Html) ->
  Expr -> m Html
renderStatement_Print fVal val = do
  valHtml <- fVal val
  pure $
    Html.div ! Attr.class_ "syntax-branch statement print" $ do
      Html.div ! Attr.class_ "syntax-line" $ do
        Html.div ! Attr.class_ "syntax-keyword" $ "print"
        Html.div ! Attr.class_ "syntax-symbol" $ ":"
        valHtml

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
    Print val ->
      runIdentity $
      renderStatement_Print
        (pure . renderExpr)
        val
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
  Html.div ! Attr.class_ "syntax-leaf expression hole" $ "_"

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
