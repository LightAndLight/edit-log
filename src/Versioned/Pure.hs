{-# language GADTs #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language InstanceSigs #-}
{-# language TupleSections #-}
module Versioned.Pure
  ( VersionedT
  , runVersionedT
  , Versioned
  , newVersioned
  )
where

import Control.Monad.State (StateT, runStateT, gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldlM)
import Data.Functor.Identity (Identity(..))

import Node (KnownHashType, HashType(..), Hash, hashType)
import Versioned (MonadVersioned(..))
import Log (Entry(..), append, getEntries, getPhysicalTime)
import Log.Pure (LogT, runLogT, Log, newLog)
import Path (Path(..), set)
import Store (followPath, addExpr, addStatement, addBlock, rebuild)
import Store.Pure (StoreT, runStoreT, Store, newStore)

data Context a
  = Context
  { initial :: a
  , root :: Hash a
  } deriving Show

newtype VersionedT a m b
  = VersionedT { unVersionedT :: StateT (Context a) (StoreT (LogT a m)) b }
  deriving (Functor, Applicative, Monad)

runVersionedT :: Monad m => Versioned a -> VersionedT a m b -> m (b, Versioned a)
runVersionedT (Versioned s l ctx) m = do
  (((res, ctx'), store'), log') <- runLogT l . runStoreT s . flip runStateT ctx $ unVersionedT m
  pure (res, Versioned store' log' ctx')

data Versioned a = Versioned Store (Log a) (Context a)
  deriving Show

newVersioned :: forall a. KnownHashType a => a -> Versioned a
newVersioned a = Versioned store newLog ctx
  where
    Identity (initialh, store) =
      runStoreT newStore $ do
        case hashType @a of
          TExpr -> addExpr a
          TStatement -> addStatement a
          TBlock -> addBlock a
    ctx = Context { initial = a, root = initialh }

instance Monad m => MonadVersioned a (VersionedT a m) where
  replace :: forall b. KnownHashType b => Path a b -> b -> VersionedT a m ()
  replace path value =
    VersionedT $ do
      rooth <- gets root
      m_targeth <- lift $ followPath path rooth
      case m_targeth of
        Nothing -> pure ()
        Just targeth -> do
          valueh :: Hash b <-
            case hashType @b of
              TExpr -> lift $ addExpr value
              TStatement -> lift $ addStatement value
              TBlock -> lift $ addBlock value
          lift . lift $ append (Replace path targeth valueh)
          case path of
            Nil -> modify $ \s -> s { root = valueh }
            _ -> pure ()

  snapshot =
    VersionedT $ do
      i <- gets initial
      entries <- lift . lift $ getEntries
      res <-
        lift $
        foldlM
          (\acc (_, entry) ->
            case entry of
              Replace path _ newh -> do
                m_new <- rebuild newh
                case m_new of
                  Nothing -> error "corrupt log: hash not in store"
                  Just new -> pure $ set path new acc
          )
          i
          entries
      (, res) <$> lift (lift getPhysicalTime)
