{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
module Diff where

import Data.Foldable (foldlM)
import Data.Type.Equality ((:~:)(..))

import qualified Log
import Node (Hash)
import Path (Level, Path(..), eqLevel)
import qualified Path
import Store (MonadStore, setH)
import qualified Store

data Entry a where
  Entry :: Level a b -> Diff b -> Entry a
deriving instance Show (Entry a)

data Change a
  = Replace { changeOld :: Hash a, changeNew :: Hash a }
deriving instance Show (Change a)

data Diff a where
  Leaf :: Change a -> Diff a
  Branch :: [Entry a] -> Diff a
deriving instance Show (Diff a)

emptyDiff :: Diff a
emptyDiff = Branch []

insert :: MonadStore m => Path a b -> Change b -> Diff a -> m (Diff a)
insert p change m =
  case p of
    Nil -> pure $ Leaf change
    Cons l p' ->
      case m of
        Leaf changeOuter ->
          case changeOuter of
            Replace old newOuter ->
              case change of
                Replace _ newInner -> do
                  m_res <- setH p (pure newInner) newOuter
                  pure $ case m_res of
                    Nothing -> m
                    Just res -> Leaf $ Replace old (Store.rootHash res)
        Branch ms -> Branch <$> entriesInsert l p' change ms
  where
    entriesInsert :: MonadStore m => Level a b -> Path b c -> Change c -> [Entry a] -> m [Entry a]
    entriesInsert level path hash entries =
      case entries of
        [] -> pure []
        entry@(Entry level' d) : rest ->
          case eqLevel level level' of
            Nothing -> (entry :) <$> entriesInsert level path hash rest
            Just Refl ->
              (\m' -> Entry level m' : rest) <$>
              insert path hash d

traversal ::
  forall a b.
  Path a b ->
  forall f. Applicative f => (Change b -> f (Change b)) -> Diff a -> f (Diff a)
traversal p f m =
  case p of
    Nil ->
      case m of
        Leaf h -> Leaf <$> f h
        Branch{} -> pure m
    Cons l p' ->
      case m of
        Leaf{} -> pure m
        Branch ms ->
          Branch <$>
          traverse
            (\entry@(Entry l' m') ->
              case eqLevel l l' of
                Nothing -> pure entry
                Just Refl -> Entry l' <$> traversal p' f m'
            )
            ms

toDiff :: MonadStore m => [Log.Entry a] -> m (Diff a)
toDiff =
  foldlM
    (\acc entry ->
       case entry of
         Log.Replace path old new ->
           insert path (Replace old new) acc
    )
    emptyDiff

fromDiff :: forall a. Diff a -> [Log.Entry a]
fromDiff = go Nil
  where
    go :: forall b. Path a b -> Diff b -> [Log.Entry a]
    go path d =
      case d of
        Leaf change ->
          case change of
            Replace old new -> [Log.Replace path old new]
        Branch entries ->
          entries >>= \(Entry level d') -> go (Path.snoc path level) d'
