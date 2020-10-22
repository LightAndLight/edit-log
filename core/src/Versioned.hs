{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language FlexibleInstances, UndecidableInstances #-}
module Versioned where

import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)

import Hash (Hash)
import Log (Time, Entry)
import NodeType (KnownNodeType)
import Path (Path)
import Sequence (IsSequence, Item)

class Monad m => MonadVersioned a m | m -> a where
  replace ::
    (KnownNodeType a, KnownNodeType b) =>
    Path a b ->
    b ->
    m (Maybe (Time, Entry a))

  replaceH ::
    (KnownNodeType a, KnownNodeType b) =>
    Path a b ->
    Hash b ->
    m (Maybe (Time, Entry a))

  insert ::
    (KnownNodeType a, KnownNodeType b, KnownNodeType (Item b), IsSequence b) =>
    Path a b ->
    (Int, Item b) ->
    m (Maybe (Time, Entry a))

  insertH ::
    (KnownNodeType a, KnownNodeType b, IsSequence b) =>
    Path a b ->
    (Int, Hash (Item b)) ->
    m (Maybe (Time, Entry a))

  delete ::
    (KnownNodeType a, KnownNodeType b, IsSequence b) =>
    Path a b ->
    Int ->
    m (Maybe (Time, Entry a))

  snapshot :: m (Time, a)

  getRoot :: m (Hash a)

instance MonadVersioned a m => MonadVersioned a (StateT s m) where
  replace p a = lift $ replace p a
  replaceH p h = lift $ replaceH p h

  insert p a = lift $ insert p a
  insertH p a = lift $ insertH p a

  delete p = lift . delete p

  snapshot = lift snapshot

  getRoot = lift getRoot
