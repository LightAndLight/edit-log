{-# language GADTs #-}
{-# language InstanceSigs, DefaultSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables, TypeApplications #-}
module Store where

import Control.Applicative (empty)
import Control.Lens.Fold ((^?), Fold)
import Control.Lens.Prism (Prism')
import Control.Lens.Review (review)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import Hash (Hash)
import Node (Node(..), _NArgs, _NParams)
import NodeType (KnownNodeType, NodeType(..), nodeType)
import Path (Path(..), Level(..))
import Syntax (Expr(..), Statement(..), Block(..), Ident(..), Args(..), Params(..))

class Monad m => MonadStore m where
  lookupNode :: Hash a -> m (Maybe (Node a))
  default lookupNode :: (m ~ t n, MonadTrans t, MonadStore n) => Hash a -> m (Maybe (Node a))
  lookupNode = lift . lookupNode

  lookupHash :: Node a -> m (Maybe (Hash a))
  default lookupHash :: (m ~ t n, MonadTrans t, MonadStore n) => Node a -> m (Maybe (Hash a))
  lookupHash = lift . lookupHash

  addNode :: KnownNodeType a => Node a -> m (Hash a)
  default addNode :: (m ~ t n, MonadTrans t, MonadStore n, KnownNodeType a) => Node a -> m (Hash a)
  addNode = lift . addNode

instance MonadStore m => MonadStore (StateT s m)
instance MonadStore m => MonadStore (ReaderT r m)
instance MonadStore m => MonadStore (ExceptT e m)
instance MonadStore m => MonadStore (MaybeT m)

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
            For_Ident ->
              case n of
                NFor identh exprh bodyh -> do
                  identh' <- go rest f identh
                  lift . addNode $ NFor identh' exprh bodyh
                _ -> empty
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

            Print_Value ->
              case n of
                NPrint valh -> do
                  valh' <- go rest f valh
                  lift . addNode $ NPrint valh'
                _ -> empty

            Return_Value ->
              case n of
                NReturn valh -> do
                  valh' <- go rest f valh
                  lift . addNode $ NReturn valh'
                _ -> empty

            Def_Name ->
              case n of
                NDef nameh args bodyh -> do
                  nameh' <- go rest f nameh
                  lift . addNode $ NDef nameh' args bodyh
                _ -> empty
            Def_Args ->
              case n of
                NDef nameh argsh bodyh -> do
                  argsh' <- go rest f argsh
                  lift . addNode $ NDef nameh argsh' bodyh
                _ -> empty
            Def_Body ->
              case n of
                NDef nameh args bodyh -> do
                  bodyh' <- go rest f bodyh
                  lift . addNode $ NDef nameh args bodyh'
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

            Call_Function -> do
              case n of
                NCall funch argsh -> do
                  funch' <- go rest f funch
                  lift . addNode $ NCall funch' argsh
                _ -> empty
            Call_Args -> do
              case n of
                NCall funch argsh -> do
                  argsh' <- go rest f argsh
                  lift . addNode $ NCall funch argsh'
                _ -> empty

            Block_Index ix -> do
              case n of
                NBlock sts | 0 <= ix && ix < length sts -> do
                  let (prefix, more) = List.splitAt ix $ NonEmpty.toList sts
                  case more of
                    [] -> error "impossible"
                    elh : suffix -> do
                      elh' <- go rest f elh
                      lift . addNode $ NBlock (foldr NonEmpty.cons (elh' NonEmpty.:| suffix) prefix)
                _ -> empty

            Args_Index ix ->
              MaybeT $ modifyHList _NArgs ix rest f n
            Params_Index ix ->
              MaybeT $ modifyHList _NParams ix rest f n

modifyHList ::
  (KnownNodeType a, MonadStore m) =>
  Prism' (Node a) [Hash b] ->
  Int ->
  Path b c ->
  (Hash c -> m (Hash c)) ->
  Node a ->
  m (Maybe (Hash a))
modifyHList _Ctor ix path f n =
  runMaybeT $
  case n ^? _Ctor of
    Just xs | 0 <= ix && ix < length xs -> do
      let (prefix, more) = List.splitAt ix xs
      case more of
        [] -> error "impossible"
        elh : suffix -> do
          elh' <- MaybeT $ modifyH path f elh
          lift . addNode $ review _Ctor (prefix ++ elh' : suffix)
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
            For_Ident ->
              case n of
                NFor identh exprh bodyh -> do
                  res <- go rest mval identh
                  rooth' <- lift . addNode $ NFor (rootHash res) exprh bodyh
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            For_Expr ->
              case n of
                NFor identh exprh bodyh -> do
                  res <- go rest mval exprh
                  rooth' <- lift . addNode $ NFor identh (rootHash res) bodyh
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            For_Block ->
              case n of
                NFor identh exprh bodyh -> do
                  res <- go rest mval bodyh
                  rooth' <- lift . addNode $ NFor identh exprh (rootHash res)
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

            Print_Value ->
              case n of
                NPrint valh -> do
                  res <- go rest mval valh
                  rooth' <- lift . addNode $ NPrint (rootHash res)
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty

            Return_Value ->
              case n of
                NReturn valh -> do
                  res <- go rest mval valh
                  rooth' <- lift . addNode $ NReturn (rootHash res)
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty

            Def_Name ->
              case n of
                NDef nameh args bodyh -> do
                  res <- go rest mval nameh
                  rooth' <- lift . addNode $ NDef (rootHash res) args bodyh
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            Def_Args ->
              case n of
                NDef nameh argsh bodyh -> do
                  res <- go rest mval argsh
                  rooth' <- lift . addNode $ NDef nameh (rootHash res) bodyh
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            Def_Body ->
              case n of
                NDef nameh args bodyh -> do
                  res <- go rest mval bodyh
                  rooth' <- lift . addNode $ NDef nameh args (rootHash res)
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

            Call_Function -> do
              case n of
                NCall funch argsh -> do
                  res <- go rest mval funch
                  rooth' <- lift . addNode $ NCall (rootHash res) argsh
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty
            Call_Args -> do
              case n of
                NCall funch argsh -> do
                  res <- go rest mval argsh
                  rooth' <- lift . addNode $ NCall funch (rootHash res)
                  pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty

            Block_Index ix -> do
              case n of
                NBlock sts | 0 <= ix && ix < length sts -> do
                  let (prefix, more) = List.splitAt ix $ NonEmpty.toList sts
                  case more of
                    [] -> error "impossible"
                    elh : suffix -> do
                      res <- go rest mval elh
                      rooth' <- lift . addNode $ NBlock (foldr NonEmpty.cons (rootHash res NonEmpty.:| suffix) prefix)
                      pure $ SetH { rootHash = rooth', targetHash = targetHash res, valueHash = valueHash res }
                _ -> empty

            Args_Index ix ->
              MaybeT $ setHList _NArgs ix rest mval n
            Params_Index ix ->
              MaybeT $ setHList _NParams ix rest mval n

setHList ::
  (KnownNodeType a, MonadStore m) =>
  Prism' (Node a) [Hash b] ->
  Int ->
  Path b c ->
  m (Hash c)->
  Node a ->
  m (Maybe (SetH a c))
setHList _Ctor ix path mval n =
  runMaybeT $
  case n ^? _Ctor of
    Just xs | 0 <= ix && ix < length xs -> do
      let (prefix, more) = List.splitAt ix xs
      case more of
        [] -> error "impossible"
        elh : suffix -> do
          res <- MaybeT $ setH path mval elh
          rooth' <- lift . addNode $ review _Ctor (prefix ++ rootHash res : suffix)
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
            addNode . NBlock $ NonEmpty.fromList (insertAll sholeHash positions $ NonEmpty.toList sts)

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
              For <$>
              go ident <*>
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
            NPrint val ->
              Print <$> go val
            NReturn val ->
              Return <$> go val
            NDef name args body ->
              Def <$>
              go name <*>
              go args <*>
              go body

            NBool b -> pure $ Bool b
            NInt n -> pure $ Int n
            NBinOp op l r ->
              BinOp op <$>
              go l <*>
              go r
            NUnOp op ex ->
              UnOp op <$> go ex
            NCall func args ->
              Call <$> go func <*> go args
            NEIdent i ->
              pure $ EIdent i

            NArgs xs -> Args <$> traverse go xs
            NParams xs -> Params <$> traverse go xs

            NBlock sts ->
              Block <$> traverse go sts

            NIdent i ->
              pure $ Ident i

            NSHole -> pure SHole
            NEHole -> pure EHole
            NIHole -> pure IHole

addIdent :: MonadStore m => Ident -> m (Hash Ident)
addIdent i =
  case i of
    Ident n -> addNode $ NIdent n
    IHole -> addNode NIHole

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
    Call func args -> do
      funch <- addExpr func
      argsh <- addArgs args
      addNode $ NCall funch argsh
    EIdent i -> addNode $ NEIdent i
    EHole -> addNode NEHole

addStatement :: MonadStore m => Statement -> m (Hash Statement)
addStatement s =
  case s of
    For ident expr body -> do
      identh <- addIdent ident
      exprh <- addExpr expr
      bodyh <- addBlock body
      addNode $ NFor identh exprh bodyh
    IfThen cond then_ -> do
      condh <- addExpr cond
      then_h <- addBlock then_
      addNode $ NIfThen condh then_h
    IfThenElse cond then_ else_ -> do
      condh <- addExpr cond
      then_h <- addBlock then_
      else_h <- addBlock else_
      addNode $ NIfThenElse condh then_h else_h
    Print val -> do
      valh <- addExpr val
      addNode $ NPrint valh
    Return val -> do
      valh <- addExpr val
      addNode $ NReturn valh
    Def name args body -> do
      nameh <- addIdent name
      argsh <- addParams args
      bodyh <- addBlock body
      addNode $ NDef nameh argsh bodyh
    SHole -> addNode NSHole

addArgs :: MonadStore m => Args -> m (Hash Args)
addArgs (Args xs) = do
  xsh <- traverse addExpr xs
  addNode $ NArgs xsh

addParams :: MonadStore m => Params -> m (Hash Params)
addParams (Params xs) = do
  xsh <- traverse addIdent xs
  addNode $ NParams xsh

addKnownNode :: forall a m. (KnownNodeType a, MonadStore m) => a -> m (Hash a)
addKnownNode a =
  case nodeType @a of
    TExpr -> addExpr a
    TStatement -> addStatement a
    TBlock -> addBlock a
    TIdent -> addIdent a
    TArgs -> addArgs a
    TParams -> addParams a

addBlock :: MonadStore m => Block -> m (Hash Block)
addBlock b =
  case b of
    Block sts -> do
      stsh <- traverse addStatement sts
      addNode $ NBlock stsh

getH :: MonadStore m => Path a b -> Hash a -> m (Maybe (Hash b))
getH path h =
  case path of
    Nil -> pure $ Just h
    Cons l rest -> do
      mNode <- lookupNode h
      case mNode of
        Nothing -> pure Nothing
        Just node ->
          case l of
            For_Ident ->
              case node of
                NFor ident _ _ -> getH rest ident
                _ -> pure Nothing
            For_Expr ->
              case node of
                NFor _ expr _ -> getH rest expr
                _ -> pure Nothing
            For_Block ->
              case node of
                NFor _ _ body -> getH rest body
                _ -> pure Nothing
            IfThen_Cond ->
              case node of
                NIfThen cond _ -> getH rest cond
                _ -> pure Nothing
            IfThen_Then ->
              case node of
                NIfThen _ then_ -> getH rest then_
                _ -> pure Nothing
            IfThenElse_Cond ->
              case node of
                NIfThenElse cond _ _ -> getH rest cond
                _ -> pure Nothing
            IfThenElse_Then ->
              case node of
                NIfThenElse _ then_ _ -> getH rest then_
                _ -> pure Nothing
            IfThenElse_Else ->
              case node of
                NIfThenElse _ _ else_ -> getH rest else_
                _ -> pure Nothing
            Print_Value ->
              case node of
                NPrint val -> getH rest val
                _ -> pure Nothing
            Return_Value ->
              case node of
                NReturn val -> getH rest val
                _ -> pure Nothing
            Def_Name ->
              case node of
                NDef name _ _ -> getH rest name
                _ -> pure Nothing
            Def_Args ->
              case node of
                NDef _ args _ -> getH rest args
                _ -> pure Nothing
            Def_Body ->
              case node of
                NDef _ _ body -> getH rest body
                _ -> pure Nothing
            BinOp_Left ->
              case node of
                NBinOp _ left _ -> getH rest left
                _ -> pure Nothing
            BinOp_Right ->
              case node of
                NBinOp _ _ right -> getH rest right
                _ -> pure Nothing
            UnOp_Value ->
              case node of
                NUnOp _ val -> getH rest val
                _ -> pure Nothing
            Call_Function ->
              case node of
                NCall func _ -> getH rest func
                _ -> pure Nothing
            Call_Args ->
              case node of
                NCall _ args -> getH rest args
                _ -> pure Nothing
            Block_Index ix ->
              case node of
                NBlock sts ->
                  case lookup ix . zip [0..] $ NonEmpty.toList sts of
                    Nothing -> pure Nothing
                    Just st -> getH rest st
            Args_Index ix ->
              getHList _NArgs ix rest node
            Params_Index ix ->
              getHList _NParams ix rest node

getHList ::
  MonadStore m =>
  Fold (Node a) [Hash b] ->
  Int ->
  Path b c ->
  Node a ->
  m (Maybe (Hash c))
getHList _Ctor ix path n =
  case n ^? _Ctor of
    Just xs ->
      case lookup ix $ zip [0..] xs of
        Nothing -> pure Nothing
        Just st -> getH path st
    _ -> pure Nothing
