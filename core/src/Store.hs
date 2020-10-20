{-# language GADTs #-}
{-# language InstanceSigs, DefaultSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language TupleSections #-}
module Store where

import Control.Applicative (empty)
import Control.Lens.Fold ((^?), Fold)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.List.NonEmpty as NonEmpty

import Hash (Hash)
import Node (Node(..))
import NodeType (KnownNodeType, NodeType(..), nodeType)
import Path (Path(..))
import qualified Path
import Sequence (IsSequence, Item, insertAll, deleteAt)
import Syntax (Expr(..), Statement(..), Block(..), Ident(..), Args(..), Params(..), Exprs(..))

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

modifyH :: (KnownNodeType a, MonadStore m) => Path a b -> (Hash b -> m (Hash b)) -> Hash a -> m (Maybe (Hash a))
modifyH path_ f_ = runMaybeT . go path_ f_
  where
    go :: (KnownNodeType a, MonadStore m) => Path a b -> (Hash b -> m (Hash b)) -> Hash a -> MaybeT m (Hash a)
    go path f rooth =
      case path of
        Nil -> lift $ f rooth
        Cons level path' -> do
          node <- MaybeT $ lookupNode rooth
          case Path.downLevelNode level node of
            Nothing -> empty
            Just (nextHash, mkNode) -> do
              nextHash' <- Path.withKnownLevelTarget level (go path' f nextHash)
              lift . addNode $ mkNode nextHash'

setH :: (KnownNodeType a, MonadStore m) => Path a b -> Hash b -> Hash a -> m (Maybe (Hash a, Hash b))
setH path val hash = do
  mNode <- lookupNode hash
  case mNode of
    Nothing -> pure Nothing
    Just node ->
      case path of
        Nil ->
          pure $ Just (val, hash)
        Cons level path' ->
          case Path.downLevelNode level node of
            Nothing -> pure Nothing
            Just (nextHash, mkNode) -> do
              mRes <- Path.withKnownLevelTarget level $ setH path' val nextHash
              case mRes of
                Nothing ->
                  pure Nothing
                Just (nextHash', old) -> do
                  Just . (, old) <$> addNode (mkNode nextHash')

insertH ::
  (KnownNodeType a, IsSequence b, MonadStore m) =>
  Path a b ->
  [(Int, [Hash (Item b)])] ->
  Hash a ->
  m (Maybe (Hash a))
insertH path positions =
  modifyH path $ \hash -> do
    m_node <- lookupNode hash
    case m_node of
      Nothing -> error $ "missing node for " <> show hash
      Just node ->
        case node of
          NBlock sts -> do
            sholeHash <- addNode NSHole
            addNode . NBlock $ NonEmpty.fromList (insertAll sholeHash positions $ NonEmpty.toList sts)
          NArgs xs -> do
            eholeHash <- addNode NEHole
            addNode . NArgs $ insertAll eholeHash positions xs
          NParams xs -> do
            iholeHash <- addNode NIHole
            addNode . NParams $ insertAll iholeHash positions xs
          _ -> pure hash

delete ::
  (IsSequence b, MonadStore m, KnownNodeType a) =>
  Path a b ->
  Int ->
  Hash a ->
  m (Maybe (Hash a, Hash (Item b)))
delete path ix hash = do
  mNode <- lookupNode hash
  case mNode of
    Nothing -> pure Nothing
    Just node ->
      case path of
        Nil -> do
          case node of
            NBlock sts ->
              Just . (, NonEmpty.toList sts !! ix) <$>
              case deleteAt ix $ NonEmpty.toList sts of
                [] ->
                  addBlock $ Block (pure SHole)
                st' : sts' ->
                  addNode (NBlock (st' NonEmpty.:| sts'))
            NArgs xs ->
              Just . (, xs !! ix) <$> addNode (NArgs $ deleteAt ix xs)
            NParams xs ->
              Just . (, xs !! ix) <$> addNode (NParams $ deleteAt ix xs)
            _ -> pure Nothing
        Cons level path' ->
          case Path.downLevelNode level node of
            Nothing -> pure Nothing
            Just (nextHash, mkNode) -> do
              mRes <- Path.withKnownLevelTarget level $ delete path' ix nextHash
              case mRes of
                Nothing -> pure Nothing
                Just (nextHash', deleted) ->
                  Just . (, deleted) <$> addNode (mkNode nextHash')

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
            NList xs -> List <$> go xs
            NEIdent i ->
              pure $ EIdent i

            NExprs xs -> Exprs <$> traverse go xs
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
    List xs -> do
      xsh <- addExprs xs
      addNode $ NList xsh
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

addExprs :: MonadStore m => Exprs -> m (Hash Exprs)
addExprs (Exprs xs) = do
  xsh <- traverse addExpr xs
  addNode $ NExprs xsh

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
    TExprs -> addExprs a
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
          case Path.downLevelNode l node of
            Nothing -> pure Nothing
            Just (nexth, _) ->
              getH rest nexth

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
