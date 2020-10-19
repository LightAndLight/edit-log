{-# language QuantifiedConstraints, StandaloneDeriving #-}
module Path.Trie.Internal where

import Data.Bifunctor.Flip (Flip(..))
import Data.Dependent.Map (DMap)

import Path (Level)

data Trie a f
  = Trie (Maybe (f a)) (DMap (Level a) (Flip Trie f))
deriving instance (Show a, forall x. Show x => Show (f x)) => Show (Trie a f)
deriving instance (Eq a, forall x. Eq x => Eq (f x)) => Eq (Trie a f)
