{-# language FlexibleContexts #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GADTs #-}
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
  -- , debugLog
  )
where

import Control.Monad.State (StateT, runStateT, gets, modify)
import Data.Functor.Identity (Identity(..))

import Hash (Hash)
import NodeType (KnownNodeType)
import Versioned (MonadVersioned(..))
import Log (MonadLog, Time, Entry(..), append, getPhysicalTime)
import Log.Pure (LogT, runLogT, Log, newLog)
import Path (Path(..))
import Sequence (IsSequence, Item)
import Store (MonadStore, rebuild)
import qualified Store
import Store.Pure (StoreT, runStoreT, Store, newStore)

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

{-
data DebugEntry a where
  DebugReplace :: Show b => Path a b -> b -> b -> DebugEntry a
  DebugInsert :: Path a Block -> Int -> Statement -> DebugEntry a
  DebugDelete :: Path a b -> Int -> DeleteItem b -> DebugEntry a
deriving instance Show (DebugEntry a)

debugLog :: KnownNodeType a => Versioned a -> [(Time, DebugEntry a)]
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
-}

newVersioned :: forall a. KnownNodeType a => a -> Versioned a
newVersioned a = Versioned store newLog ctx
  where
    Identity (initialh, store) =
      runStoreT newStore $ Store.addKnownNode a
    ctx = Context { initial = a, root = initialh }

instance Monad m => MonadVersioned a (VersionedT a m) where
  getRoot = VersionedT $ gets root

  replace :: forall b. (KnownNodeType a, KnownNodeType b) => Path a b -> b -> VersionedT a m (Maybe (Time, Entry a))
  replace path value = do
    rooth <- getRoot
    valh <- Store.addKnownNode @b value
    m_res <-
      Store.setH
        path
        valh
        rooth
    case m_res of
      Nothing -> pure Nothing
      Just (rooth', oldh) -> do
        let entry = Replace path oldh valh
        t <- append entry
        VersionedT $ modify $ \s -> s { root = rooth' }
        pure $ Just (t, entry)

  replaceH path valueh = do
    rooth <- getRoot
    m_res <- Store.setH path valueh rooth
    case m_res of
      Nothing -> pure Nothing
      Just (rooth', oldh) -> do
        let entry = Replace path oldh valueh
        t <- append entry
        VersionedT . modify $ \s -> s { root = rooth' }
        pure $ Just (t, entry)

  insert :: (KnownNodeType a, KnownNodeType (Item b), IsSequence b) => Path a b -> (Int, Item b) -> VersionedT a m (Maybe (Time, Entry a))
  insert path (ix, x) = do
    xh <- Store.addKnownNode x
    insertH path (ix, xh)

  insertH :: (KnownNodeType a, IsSequence b) => Path a b -> (Int, Hash (Item b)) -> VersionedT a m (Maybe (Time, Entry a))
  insertH path (ix, xh) = do
    rooth <- getRoot
    m_rooth' <- Store.insertH path [(ix, [xh])] rooth
    case m_rooth' of
      Nothing -> pure Nothing
      Just rooth' -> do
        let entry = Insert path ix xh
        t <- append entry
        VersionedT $ modify $ \s -> s { root = rooth' }
        pure $ Just (t, entry)

  delete :: (KnownNodeType a, IsSequence b) => Path a b -> Int -> VersionedT a m (Maybe (Time, Entry a))
  delete path ix = do
    rooth <- getRoot
    mRes <- Store.delete path ix rooth
    case mRes of
      Nothing -> pure Nothing
      Just (rooth', deleted)  -> do
        let entry = Delete path ix deleted
        t <- append entry
        VersionedT $ modify $ \s -> s { root = rooth' }
        pure $ Just (t, entry)

  snapshot = do
    m_res <- rebuild =<< getRoot
    case m_res of
      Nothing -> error "corrupt log: missing a hash"
      Just res -> (, res) <$> getPhysicalTime
