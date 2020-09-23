{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Log where

import Node (Hash)
import Path (Path)

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
deriving instance Show (Entry a)

class Monad m => MonadLog a m | m -> a where
  append :: Entry a -> m Time

  getEntry :: Time -> m (Maybe (Entry a))
  getEntries :: m [(Time, Entry a)]
  getPhysicalTime :: m Time
