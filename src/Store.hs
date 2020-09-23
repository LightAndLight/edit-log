{-# language GADTs #-}
module Store where

import Control.Applicative (empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.List as List

import Path (Path(..), Level(..))
import Syntax (Expr(..), Statement(..), Block(..))
import Node (KnownHashType, Hash, Node(..), hashNode)

class Monad m => MonadStore m where
  lookupNode :: Hash a -> m (Maybe (Node a))
  lookupHash :: Node a -> m (Maybe (Hash a))

  write :: Hash a -> Node a -> m ()

data SetH a b
  = SetH
  { rootHash :: Hash a
  , targetHash :: Hash b
  , valueHash :: Hash b
  }

setH :: MonadStore m => Path a b -> m (Hash b)-> Hash a -> m (Maybe (SetH a b))
setH path_ val_ = runMaybeT . go path_ val_
  where
    go :: MonadStore m => Path a b -> m (Hash b) -> Hash a -> MaybeT m (SetH a b)
    go path mval rooth =
      case path of
        Nil -> do
          valh <- lift mval
          pure $ SetH { rootHash = valh, targetHash = rooth, valueHash = valh }
        Cons l rest -> do
          n <- MaybeT $ lookupNode rooth
          case l of
            For_Ident -> empty
            For_Expr ->
              case n of
                NFor ident exprh bodyh -> do
                  res <- go rest mval exprh
                  rooth' <- lift . addNode $ NFor ident (rootHash res) bodyh
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            For_Block ->
              case n of
                NFor ident exprh bodyh -> do
                  res <- go rest mval bodyh
                  rooth' <- lift . addNode $ NFor ident exprh (rootHash res)
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty

            IfThen_Cond ->
              case n of
                NIfThen condh then_h -> do
                  res <- go rest mval condh
                  rooth' <- lift . addNode $ NIfThen (rootHash res) then_h
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            IfThen_Then ->
              case n of
                NIfThen condh then_h -> do
                  res <- go rest mval then_h
                  rooth' <- lift . addNode $ NIfThen condh (rootHash res)
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty

            IfThenElse_Cond ->
              case n of
                NIfThenElse condh then_h else_h -> do
                  res <- go rest mval condh
                  rooth' <- lift . addNode $ NIfThenElse (rootHash res) then_h else_h
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            IfThenElse_Then ->
              case n of
                NIfThenElse condh then_h else_h -> do
                  res <- go rest mval then_h
                  rooth' <- lift . addNode $ NIfThenElse condh (rootHash res) else_h
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            IfThenElse_Else ->
              case n of
                NIfThenElse condh then_h else_h -> do
                  res <- go rest mval else_h
                  rooth' <- lift . addNode $ NIfThenElse condh then_h (rootHash res)
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty

            BinOp_Left ->
              case n of
                NBinOp op lefth righth -> do
                  res <- go rest mval lefth
                  rooth' <- lift . addNode $ NBinOp op (rootHash res) righth
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            BinOp_Right ->
              case n of
                NBinOp op lefth righth -> do
                  res <- go rest mval righth
                  rooth' <- lift . addNode $ NBinOp op lefth (rootHash res)
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty

            UnOp_Value -> do
              case n of
                NUnOp op valueh -> do
                  res <- go rest mval valueh
                  rooth' <- lift . addNode $ NUnOp op (rootHash res)
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty

            Block_Index ix -> do
              case n of
                NBlock sts | 0 <= ix && ix < length sts -> do
                  let (prefix, more) = List.splitAt ix sts
                  case more of
                    [] -> error "impossible"
                    elh : suffix -> do
                      res <- go rest mval elh
                      rooth' <- lift . addNode $ NBlock (prefix ++ rootHash res : suffix)
                      pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
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
