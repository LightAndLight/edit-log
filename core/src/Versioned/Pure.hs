{-# language GADTs #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language InstanceSigs #-}
{-# language TupleSections #-}
{-# language StandaloneDeriving #-}
module Versioned.Pure
  ( VersionedT
  , runVersionedT
  , Versioned
  , newVersioned
  , debugLog
  )
where

import Control.Monad.State (StateT, runStateT, gets, modify)
import Data.Functor.Identity (Identity(..))

import Hash (Hash)
import NodeType (KnownNodeType, NodeType(..), nodeType)
import Versioned (MonadVersioned(..))
import Log (MonadLog, Time, Entry(..), append, getEntries, getPhysicalTime)
import Log.Pure (LogT, runLogT, Log, newLog)
import Path (Path(..))
import qualified Path
import Store (MonadStore, addExpr, addStatement, addBlock, rebuild)
import qualified Store
import Store.Pure (StoreT, runStoreT, Store, newStore)
import Syntax (Statement, Block)

data Context a
  = Context
  { initial :: a
  , root :: Hash a
  } deriving Show

newtype VersionedT a m b
  = VersionedT { unVersionedT :: StateT (Context a) (StoreT (LogT a m)) b }
  deriving (Functor, Applicative, Monad, MonadStore, MonadLog a)

runVersionedT :: Monad m => Versioned a -> VersionedT a m b -> m (b, Versioned a)
runVersionedT (Versioned s l ctx) m = do
  (((res, ctx'), store'), log') <- runLogT l . runStoreT s . flip runStateT ctx $ unVersionedT m
  pure (res, Versioned store' log' ctx')

data Versioned a = Versioned Store (Log a) (Context a)
  deriving Show

data DebugEntry a where
  DebugReplace :: Show b => Path a b -> b -> b -> DebugEntry a
  DebugInsert :: Path a Block -> Int -> Statement -> DebugEntry a
  DebugDelete :: Path a Block -> Int -> Statement -> DebugEntry a
deriving instance Show (DebugEntry a)

debugLog :: Show a => Versioned a -> [(Time, DebugEntry a)]
debugLog (Versioned s l _) = (fmap.fmap) f entries
  where
    f entry =
      case entry of
        Replace path oldh newh ->
          let
            Identity ((m_old, m_new), _) = runStoreT s $ do
              (,) <$> rebuild oldh <*> rebuild newh
          in
            case (m_old, m_new) of
              (Nothing, _) -> error "impossible"
              (_, Nothing) -> error "impossible"
              (Just old, Just new) -> Path.showingPathTarget path (DebugReplace path old new)
        Insert path ix newh ->
          let
            Identity (m_new, _) = runStoreT s $ rebuild newh
          in
            case m_new of
              Nothing -> error "impossible"
              Just new -> Path.showingPathTarget path (DebugInsert path ix new)
        Delete path ix oldh ->
          let
            Identity (m_old, _) = runStoreT s $ rebuild oldh
          in
            case m_old of
              Nothing -> error "impossible"
              Just old -> Path.showingPathTarget path (DebugDelete path ix old)
    Identity (entries, _) = runLogT l getEntries

newVersioned :: forall a. KnownNodeType a => a -> Versioned a
newVersioned a = Versioned store newLog ctx
  where
    Identity (initialh, store) =
      runStoreT newStore $ do
        case nodeType @a of
          TExpr -> addExpr a
          TStatement -> addStatement a
          TBlock -> addBlock a
    ctx = Context { initial = a, root = initialh }

instance Monad m => MonadVersioned a (VersionedT a m) where
  replace :: forall b. KnownNodeType b => Path a b -> b -> VersionedT a m (Maybe (Time, Entry a))
  replace path value = do
    rooth <- VersionedT $ gets root
    m_res <-
      Store.setH
        path
        (case nodeType @b of
            TExpr -> addExpr value
            TStatement -> addStatement value
            TBlock -> addBlock value
        )
        rooth
    case m_res of
      Nothing -> pure Nothing
      Just res -> do
        let entry = Replace path (Store.targetHash res) (Store.valueHash res)
        t <- append entry
        VersionedT $ modify $ \s -> s { root = Store.rootHash res }
        pure $ Just (t, entry)

  replaceH path valueh = do
    rooth <- VersionedT $ gets root
    m_res <- Store.setH path (pure valueh) rooth
    case m_res of
      Nothing -> pure Nothing
      Just res -> do
        let entry = Replace path (Store.targetHash res) (Store.valueHash res)
        t <- append entry
        VersionedT . modify $ \s -> s { root = Store.rootHash res }
        pure $ Just (t, entry)

  insert :: Path a Block -> (Int, Statement) -> VersionedT a m (Maybe (Time, Entry a))
  insert path (ix, st) = do
    sth <- addStatement st
    insertH path (ix, sth)

  insertH :: Path a Block -> (Int, Hash Statement) -> VersionedT a m (Maybe (Time, Entry a))
  insertH path (ix, sth) = do
    rooth <- VersionedT $ gets root
    m_rooth' <- Store.insertH path [(ix, [sth])] rooth
    case m_rooth' of
      Nothing -> pure Nothing
      Just rooth' -> do
        let entry = Insert path ix sth
        t <- append entry
        VersionedT $ modify $ \s -> s { root = rooth' }
        pure $ Just (t, entry)

  snapshot = do
    m_res <- rebuild =<< VersionedT (gets root)
    case m_res of
      Nothing -> error "corrupt log: missing a hash"
      Just res -> (, res) <$> getPhysicalTime
