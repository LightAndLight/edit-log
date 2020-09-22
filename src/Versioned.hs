{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Versioned where

import Log (Time)
import Node (KnownHashType)
import Path (Path)

class Monad m => MonadVersioned a m | m -> a where
  replace :: KnownHashType b => Path a b -> b -> m ()

  snapshot :: m (Time, a)
