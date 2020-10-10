{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language FlexibleInstances, UndecidableInstances #-}
module Versioned where

import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)

import Hash (Hash)
import Log (Time, Entry)
import NodeType (KnownNodeType)
import Path (Path)
import Syntax (Statement, Block)

class Monad m => MonadVersioned a m | m -> a where
  replace :: KnownNodeType b => Path a b -> b -> m (Maybe (Time, Entry a))
  replaceH :: Path a b -> Hash b -> m (Maybe (Time, Entry a))

  insert :: Path a Block -> (Int, Statement) -> m (Maybe (Time, Entry a))
  insertH :: Path a Block -> (Int, Hash Statement) -> m (Maybe (Time, Entry a))

  snapshot :: m (Time, a)

  getRoot :: m (Hash a)

instance MonadVersioned a m => MonadVersioned a (StateT s m) where
  replace p a = lift $ replace p a
  replaceH p h = lift $ replaceH p h

  insert p a = lift $ insert p a
  insertH p a = lift $ insertH p a

  snapshot = lift snapshot

  getRoot = lift getRoot
