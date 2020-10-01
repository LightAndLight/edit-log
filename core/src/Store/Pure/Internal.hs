{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language QuantifiedConstraints, UndecidableInstances #-}
module Store.Pure.Internal where

import Hash (Hash)
import Node (Node)

data Entry f g where
  Entry :: f a -> g a -> Entry f g
deriving instance (forall x. Show (f x), forall y. Show (g y)) => Show (Entry f g)

data Store
  = Store
  { forward :: [Entry Hash Node]
  , backward :: [Entry Node Hash]
  } deriving Show
