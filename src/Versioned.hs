{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language FlexibleInstances, UndecidableInstances #-}
module Versioned where

import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)

import Log (Time, Entry)
import Node (KnownHashType, Hash)
import Path (Path)

class Monad m => MonadVersioned a m | m -> a where
  replace :: KnownHashType b => Path a b -> b -> m (Maybe (Time, Entry a))
  replaceH :: Path a b -> Hash b -> m (Maybe (Time, Entry a))

  snapshot :: m (Time, a)

instance MonadVersioned a m => MonadVersioned a (StateT s m) where
  replace p a = lift $ replace p a
  replaceH p h = lift $ replaceH p h
  snapshot = lift snapshot