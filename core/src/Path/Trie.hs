{-# language GADTs #-}
module Path.Trie
  ( Trie
  , empty
  , insert
  , lookup
  , down
  )
where

import Prelude hiding (lookup)
import Data.Bifunctor.Flip (Flip(..))
import qualified Data.Dependent.Map as DMap

import Path (Path(..), Level)
import Path.Trie.Internal (Trie(..))

empty :: Trie a f
empty = Trie Nothing mempty

insert :: Path a b -> f b -> Trie a f -> Trie a f
insert path val (Trie mVal levels) =
  case path of
    Nil ->
      Trie (Just val) levels
    Cons level path' ->
      Trie mVal $
      DMap.alter
        (\mExisting ->
           Just . Flip $ case mExisting of
             Nothing -> insert path' val empty
             Just (Flip existing) -> insert path' val existing
        )
        level
        levels

lookup :: Path a b -> Trie a f -> Maybe (f b)
lookup path (Trie mVal levels) =
  case path of
    Nil ->
      mVal
    Cons level path' ->
      DMap.lookup level levels >>= lookup path' . runFlip

down :: Level a b -> Trie a f -> Maybe (Trie b f)
down level (Trie _ levels) = runFlip <$> DMap.lookup level levels
