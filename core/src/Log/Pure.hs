{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GeneralizedNewtypeDeriving #-}
module Log.Pure
  ( LogT
  , runLogT
  , Log
  , newLog
  )
where

import Control.Monad.State (StateT, runStateT, get, gets, put)
import qualified Data.List as List

import Log (MonadLog(..), Entry(..), Time, zero, tick)

newtype LogT a m b = LogT { unLogT :: StateT (Log a) m b }
  deriving (Functor, Applicative, Monad)

runLogT :: Monad m => Log a -> LogT a m b -> m (b, Log a)
runLogT l m = runStateT (unLogT m) l

data Log a
  = Log
  { logTime :: Time
  , logEntries :: [(Time, Entry a)]
  } deriving Show

newLog :: Log a
newLog = Log zero []

instance Monad m => MonadLog a (LogT a m) where
  append entry =
    LogT $ do
      l <- get
      let time = logTime l
      put $ Log { logTime = tick time, logEntries = (time, entry) : logEntries l }
      pure time

  getEntry t = LogT . gets $ lookup t . logEntries
  getPhysicalTime = LogT $ gets logTime
  getEntries = LogT . gets $ List.sortOn fst . logEntries
