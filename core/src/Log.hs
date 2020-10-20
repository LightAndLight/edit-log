{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language FlexibleInstances, UndecidableInstances #-}
{-# language InstanceSigs, DefaultSignatures #-}
module Log where

import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)

import Hash (Hash)
import NodeType (KnownNodeType)
import Path (Path)
import Store.Pure (StoreT)
import Sequence (IsSequence, Item)

newtype Time = Time Int
  deriving (Eq, Ord, Show)

zero :: Time
zero = Time 0

tick :: Time -> Time
tick (Time n) = Time (n+1)

data Entry a where
  Replace ::
    KnownNodeType a =>
    Path a b ->
    Hash b -> -- old
    Hash b -> -- new
    Entry a
  Insert ::
    IsSequence b =>
    Path a b ->
    Int ->
    Hash (Item b) -> -- new
    Entry a
  Delete ::
    IsSequence b =>
    Path a b ->
    Int ->
    Hash (Item b) -> -- old
    Entry a
deriving instance Show (Entry a)

class Monad m => MonadLog a m | m -> a where
  append :: Entry a -> m Time
  default append :: (m ~ t n, MonadTrans t, MonadLog a n) => Entry a -> m Time
  append = lift . append

  getEntry :: Time -> m (Maybe (Entry a))
  default getEntry :: (m ~ t n, MonadTrans t, MonadLog a n) => Time -> m (Maybe (Entry a))
  getEntry = lift . getEntry

  getEntries :: m [(Time, Entry a)]
  default getEntries :: (m ~ t n, MonadTrans t, MonadLog a n) => m [(Time, Entry a)]
  getEntries = lift getEntries

  getPhysicalTime :: m Time
  default getPhysicalTime :: (m ~ t n, MonadTrans t, MonadLog a n) => m Time
  getPhysicalTime = lift getPhysicalTime

instance MonadLog a m => MonadLog a (StateT s m)
instance MonadLog a m => MonadLog a (StoreT m)
