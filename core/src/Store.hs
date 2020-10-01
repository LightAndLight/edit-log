{-# language GADTs #-}
{-# language InstanceSigs, DefaultSignatures #-}
{-# language ScopedTypeVariables #-}
module Store where

import Control.Applicative (empty)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.State (StateT)
import Data.Function (on)
import qualified Data.List as List

import Hash (Hash)
import Node (Node(..), hashNode)
import NodeType (KnownNodeType)
import Path (Path(..), Level(..))
import Syntax (Expr(..), Statement(..), Block(..))

class Monad m => MonadStore m where
  lookupNode :: Hash a -> m (Maybe (Node a))
  default lookupNode :: (m ~ t n, MonadTrans t, MonadStore n) => Hash a -> m (Maybe (Node a))
  lookupNode = lift . lookupNode

  lookupHash :: Node a -> m (Maybe (Hash a))
  default lookupHash :: (m ~ t n, MonadTrans t, MonadStore n) => Node a -> m (Maybe (Hash a))
  lookupHash = lift . lookupHash

  write :: Hash a -> Node a -> m ()
  default write :: (m ~ t n, MonadTrans t, MonadStore n) => Hash a -> Node a -> m ()
  write h = lift . write h

instance MonadStore m => MonadStore (StateT s m)

modifyH :: MonadStore m => Path a b -> (Hash b -> m (Hash b)) -> Hash a -> m (Maybe (Hash a))
modifyH path_ f_ = runMaybeT . go path_ f_
  where
    go :: MonadStore m => Path a b -> (Hash b -> m (Hash b)) -> Hash a -> MaybeT m (Hash a)
    go path f rooth =
      case path of
        Nil -> lift $ f rooth
        Cons l rest -> do
          n <- MaybeT $ lookupNode rooth
          case l of
            For_Ident -> empty
            For_Expr ->
              case n of
                NFor ident exprh bodyh -> do
                  exprh' <- go rest f exprh
                  lift . addNode $ NFor ident exprh' bodyh
                _ -> empty
            For_Block ->
              case n of
                NFor ident exprh bodyh -> do
                  bodyh' <- go rest f bodyh
                  lift . addNode $ NFor ident exprh bodyh'
                _ -> empty

            IfThen_Cond ->
              case n of
                NIfThen condh then_h -> do
                  condh' <- go rest f condh
                  lift . addNode $ NIfThen condh' then_h
                _ -> empty
            IfThen_Then ->
              case n of
                NIfThen condh then_h -> do
                  then_h' <- go rest f then_h
                  lift . addNode $ NIfThen condh then_h'
                _ -> empty

            IfThenElse_Cond ->
              case n of
                NIfThenElse condh then_h else_h -> do
                  condh' <- go rest f condh
                  lift . addNode $ NIfThenElse condh' then_h else_h
                _ -> empty
            IfThenElse_Then ->
              case n of
                NIfThenElse condh then_h else_h -> do
                  then_h' <- go rest f then_h
                  lift . addNode $ NIfThenElse condh then_h' else_h
                _ -> empty
            IfThenElse_Else ->
              case n of
                NIfThenElse condh then_h else_h -> do
                  else_h' <- go rest f else_h
                  lift . addNode $ NIfThenElse condh then_h else_h'
                _ -> empty

            BinOp_Left ->
              case n of
                NBinOp op lefth righth -> do
                  lefth' <- go rest f lefth
                  lift . addNode $ NBinOp op lefth' righth
                _ -> empty
            BinOp_Right ->
              case n of
                NBinOp op lefth righth -> do
                  righth' <- go rest f righth
                  lift . addNode $ NBinOp op lefth righth'
                _ -> empty

            UnOp_Value -> do
              case n of
                NUnOp op valueh -> do
                  valueh' <- go rest f valueh
                  lift . addNode $ NUnOp op valueh'
                _ -> empty

            Block_Index ix -> do
              case n of
                NBlock sts | 0 <= ix && ix < length sts -> do
                  let (prefix, more) = List.splitAt ix sts
                  case more of
                    [] -> error "impossible"
                    elh : suffix -> do
                      elh' <- go rest f elh
                      lift . addNode $ NBlock (prefix ++ elh' : suffix)
                _ -> empty

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

{-

insertAll [(0, [x, y])] [a, b] = [x, y, a, b]

insertAll [(0, [x, y]), (1, [z, w])] [a, b] = [x, y, a, z, w, b]

insertAll [(1, [z, w]), (0, [x, y])] [a, b] = [x, y, a, z, w, b]

insertAll [(2, [z, w])] [a, b] = [a, b, z, w]

-}
insertAll :: forall b. b -> [(Int, [b])] -> [b] -> [b]
insertAll def inserts bs =
  -- support inserting at the tail of the list
  if maxIx >= length fullBs
  then resultsWithoutLast ++ maxEntries
  else resultsWithoutLast

  where
    (maxIx, maxEntries) = List.maximumBy (compare `on` fst) inserts

    -- if there are entries that occur after the end of the original list,
    -- pad the end of the list with an appropriate number of holes
    fullBs =
      bs ++
      replicate (maxIx - length bs) def

    -- intersperse the entries at the appropriate index
    resultsWithoutLast = do
      (ix, b) <- zip [0..] fullBs
      case lookup ix inserts of
        Nothing -> pure b
        Just moreBs -> moreBs ++ [b]

insertH :: MonadStore m => Path a Block -> [(Int, [Hash Statement])] -> Hash a -> m (Maybe (Hash a))
insertH path positions =
  modifyH path $ \blockh -> do
    m_node <- lookupNode blockh
    case m_node of
      Nothing -> error $ "missing node for " <> show blockh
      Just block ->
        case block of
          NBlock sts -> do
            sholeHash <- addNode NSHole
            addNode . NBlock $ insertAll sholeHash positions sts

addNode :: (KnownNodeType a, MonadStore m) => Node a -> m (Hash a)
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
