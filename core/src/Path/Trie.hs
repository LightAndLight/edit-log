{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
module Path.Trie
  ( Trie
  , empty
  , insert
  , lookup
  , current
  , levels
  , down
  , null
  , traverseWithPath
  , traverseWithPath_
  , foldMapWithPath
  , mapWithPath
  )
where

import Prelude hiding (lookup, null)
import Data.Bifunctor.Flip (Flip(..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Functor (void)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Ap(..))

import Path (Path(..), Level)
import qualified Path
import Path.Trie.Internal (Trie(..))

empty :: Trie a f
empty = Trie Nothing mempty

insert :: Path a b -> f b -> Trie a f -> Trie a f
insert path val (Trie mVal lvls) =
  case path of
    Nil ->
      Trie (Just val) lvls
    Cons level path' ->
      Trie mVal $
      DMap.alter
        (\mExisting ->
           Just . Flip $ case mExisting of
             Nothing -> insert path' val empty
             Just (Flip existing) -> insert path' val existing
        )
        level
        lvls

lookup :: Path a b -> Trie a f -> Maybe (f b)
lookup path (Trie mVal lvls) =
  case path of
    Nil ->
      mVal
    Cons level path' ->
      DMap.lookup level lvls >>= lookup path' . runFlip

down :: Level a b -> Trie a f -> Maybe (Trie b f)
down level (Trie _ lvls) = runFlip <$> DMap.lookup level lvls

null :: Trie a f -> Bool
null (Trie mVal lvls) =
  case mVal of
    Nothing -> DMap.null lvls
    Just{} -> False

current :: Trie a f -> Maybe (f a)
current (Trie mVal _) = mVal

levels :: Trie a f -> DMap (Level a) (Flip Trie f)
levels (Trie _ ls) = ls

traverseWithPath ::
  forall m a f g.
  Applicative m =>
  (forall x. Path a x -> f x -> m (g x)) ->
  Trie a f ->
  m (Trie a g)
traverseWithPath f = go Nil
  where
    go :: forall b. Path a b -> Trie b f -> m (Trie b g)
    go p (Trie val rest) =
      Trie <$>
      traverse (f p) val <*>
      DMap.traverseWithKey (\level (Flip nextTrie) -> Flip <$> go (Path.snoc p level) nextTrie) rest

foldMapWithPath ::
  Monoid m =>
  (forall x. Path a x -> f x -> m) ->
  Trie a f ->
  m
foldMapWithPath f = getConst . traverseWithPath (\p -> Const . f p)

traverseWithPath_ ::
  Applicative m =>
  (forall x. Path a x -> f x -> m (g x)) ->
  Trie a f ->
  m ()
traverseWithPath_ f = getAp . foldMapWithPath (\p -> void . Ap . f p)

mapWithPath ::
  (forall x. Path a x -> f x -> g x) ->
  Trie a f ->
  Trie a g
mapWithPath f = runIdentity . traverseWithPath (\p -> Identity . f p)
