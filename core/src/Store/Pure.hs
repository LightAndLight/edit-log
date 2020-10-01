{-# language GeneralizedNewtypeDeriving #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language QuantifiedConstraints, UndecidableInstances #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
module Store.Pure
  ( StoreT
  , runStoreT
  , Store
  , newStore
  )
where

import Control.Monad.State (StateT, runStateT, get, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Type.Equality ((:~:)(Refl))

import Hash (Hash, eqHash)
import Node (Node, eqNode)
import Store (MonadStore(..))

data Entry f g where
  Entry :: f a -> g a -> Entry f g
deriving instance (forall x. Show (f x), forall y. Show (g y)) => Show (Entry f g)

lookupEntry ::
  (forall a b. f a -> f b -> Maybe (a :~: b)) ->
  f x ->
  [Entry f g] ->
  Maybe (g x)
lookupEntry eq k entries =
  case entries of
    [] -> Nothing
    Entry k' v : rest ->
      case eq k k' of
        Nothing -> lookupEntry eq k rest
        Just Refl -> Just v

data Store
  = Store
  { forward :: [Entry Hash Node]
  , backward :: [Entry Node Hash]
  } deriving Show

newStore :: Store
newStore = Store [] []

newtype StoreT m a = StoreT { unStoreT :: StateT Store m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans StoreT where; lift = StoreT . lift

runStoreT :: Monad m => Store -> StoreT m a -> m (a, Store)
runStoreT s m = runStateT (unStoreT m) s

instance Monad m => MonadStore (StoreT m) where
  lookupNode h =
    StoreT $ do
      ctx <- get
      pure $ lookupEntry eqHash h (forward ctx)
  lookupHash n =
    StoreT $ do
      ctx <- get
      pure $ lookupEntry eqNode n (backward ctx)
  write h n = do
    m_n' <- lookupNode h
    StoreT $ do
      case m_n' of
        Just n' ->
          if n == n'
          then pure ()
          else error "hash collision"
        Nothing -> modify $ \s -> s { forward = Entry h n : forward s }
    m_h' <- lookupHash n
    StoreT $ do
      case m_h' of
        Just h' ->
          if h == h'
          then pure ()
          else error "writing node with inconsistent hash"
        Nothing -> modify $ \s -> s { backward = Entry n h : backward s }
