{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language FlexibleInstances, UndecidableInstances #-}
{-# language InstanceSigs, DefaultSignatures #-}
module Log where

import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)

import Node (Hash)
import Path (Path)
import Store.Pure (StoreT)
import Syntax (Block, Statement)

newtype Time = Time Int
  deriving (Eq, Ord, Show)

zero :: Time
zero = Time 0

tick :: Time -> Time
tick (Time n) = Time (n+1)

data Entry a where
  Replace ::
    Path a b ->
    Hash b -> -- old
    Hash b -> -- new
    Entry a
  Insert ::
    Path a Block ->
    Int ->
    Hash Statement -> -- new
    Entry a
  Delete ::
    Path a Block ->
    Int ->
    Hash Statement -> -- old
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
