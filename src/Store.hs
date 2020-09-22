{-# language GADTs #-}
module Store where

import Control.Applicative (empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Path (Path(..), Level(..))
import Syntax (Expr(..), Statement(..), Block(..))
import Node (KnownHashType, Hash, Node(..), hashNode)

class Monad m => MonadStore m where
  lookupNode :: Hash a -> m (Maybe (Node a))
  lookupHash :: Node a -> m (Maybe (Hash a))

  write :: Hash a -> Node a -> m ()

followPath :: MonadStore m => Path a b -> Hash a -> m (Maybe (Hash b))
followPath path_ = runMaybeT . go path_
  where
    go :: MonadStore m => Path a b -> Hash a -> MaybeT m (Hash b)
    go path h =
      case path of
        Nil -> pure h
        Cons l rest -> do
          n <- MaybeT $ lookupNode h
          case l of
            For_Ident -> empty
            For_Expr ->
              case n of
                NFor _ expr _ -> go rest expr
                _ -> empty
            For_Block ->
              case n of
                NFor _ _ block -> go rest block
                _ -> empty

            IfThen_Cond ->
              case n of
                NIfThen cond _ -> go rest cond
                _ -> empty
            IfThen_Then ->
              case n of
                NIfThen _ then_ -> go rest then_
                _ -> empty

            IfThenElse_Cond ->
              case n of
                NIfThenElse cond _ _ -> go rest cond
                _ -> empty
            IfThenElse_Then ->
              case n of
                NIfThenElse _ then_ _ -> go rest then_
                _ -> empty
            IfThenElse_Else ->
              case n of
                NIfThenElse _ _ else_ -> go rest else_
                _ -> empty

            BinOp_Left ->
              case n of
                NBinOp _ left _ -> go rest left
                _ -> empty
            BinOp_Right ->
              case n of
                NBinOp _ _ right -> go rest right
                _ -> empty

            UnOp_Value -> do
              case n of
                NUnOp _ value -> go rest value
                _ -> empty

            Block_Index ix -> do
              case n of
                NBlock sts | 0 <= ix && ix < length sts -> go rest (sts !! ix)
                _ -> empty


addNode :: (KnownHashType a, MonadStore m) => Node a -> m (Hash a)
addNode n = do
  let h = hashNode n
  write h n
  pure h

rebuild :: MonadStore m => Hash a -> m (Maybe a)
rebuild = runMaybeT . go
  where
    go :: MonadStore m => Hash a -> MaybeT m a
    go h = do
      m_node <- lift $ lookupNode h
      case m_node of
        Nothing -> empty
        Just node ->
          case node of
            NFor ident ex body ->
              For ident <$>
              go ex <*>
              go body
            NIfThen cond then_ ->
              IfThen <$>
              go cond <*>
              go then_
            NIfThenElse cond then_ else_ ->
              IfThenElse <$>
              go cond <*>
              go then_ <*>
              go else_

            NBool b -> pure $ Bool b
            NInt n -> pure $ Int n
            NBinOp op l r ->
              BinOp op <$>
              go l <*>
              go r
            NUnOp op ex ->
              UnOp op <$> go ex

            NBlock sts ->
              Block <$> traverse go sts

            NBHole -> pure BHole
            NSHole -> pure SHole
            NEHole -> pure EHole

addExpr :: MonadStore m => Expr -> m (Hash Expr)
addExpr e =
  case e of
    Bool b -> addNode $ NBool b
    Int v -> addNode $ NInt v
    BinOp op l r -> do
      lh <- addExpr l
      rh <- addExpr r
      addNode $ NBinOp op lh rh
    UnOp op value -> do
      valueh <- addExpr value
      addNode $ NUnOp op valueh
    EHole -> addNode NEHole

addStatement :: MonadStore m => Statement -> m (Hash Statement)
addStatement s =
  case s of
    For ident expr body -> do
      exprh <- addExpr expr
      bodyh <- addBlock body
      addNode $ NFor ident exprh bodyh
    IfThen cond then_ -> do
      condh <- addExpr cond
      then_h <- addBlock then_
      addNode $ NIfThen condh then_h
    IfThenElse cond then_ else_ -> do
      condh <- addExpr cond
      then_h <- addBlock then_
      else_h <- addBlock else_
      addNode $ NIfThenElse condh then_h else_h
    SHole -> addNode NSHole

addBlock :: MonadStore m => Block -> m (Hash Block)
addBlock b =
  case b of
    Block sts -> do
      stsh <- traverse addStatement sts
      addNode $ NBlock stsh
    BHole -> addNode NBHole
