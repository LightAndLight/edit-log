{-# language GADTs, KindSignatures #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
module Diff where

import Data.Foldable (foldlM)
import Data.Type.Equality ((:~:)(..))

import qualified Log
import Node (Hash)
import Path (Level(..), Path(..), eqLevel)
import qualified Path
import Store (MonadStore, setH)
import qualified Store
import Syntax (Block, Statement)

data Entry a where
  Entry :: Level a b -> Diff b -> Entry a
deriving instance Show (Entry a)

lookupEntry :: Level a b -> [Entry a] -> Maybe (Diff b)
lookupEntry level entries =
  case entries of
    [] -> Nothing
    Entry level' diff : rest ->
      case eqLevel level level' of
        Nothing -> lookupEntry level rest
        Just Refl -> Just diff

newtype InsertPositions
  = InsertPositions [(Int, [Hash Statement])]
  deriving Show

getPosition :: Int -> InsertPositions -> Maybe (Hash Statement)
getPosition ix (InsertPositions ps) = go ps
  where
    go :: [(Int, [Hash Statement])] -> Maybe (Hash Statement)
    go positions =
      case positions of
        [] -> Nothing
        (positionIndex, positionInserts) : rest
          | let adjustedIx = ix - positionIndex
          , 0 <= adjustedIx && adjustedIx < length positionInserts ->
              Just $ positionInserts !! adjustedIx
          | otherwise -> go rest

setPosition :: Int -> Hash Statement -> InsertPositions -> InsertPositions
setPosition ix val (InsertPositions ps) = InsertPositions $ go ps
  where
    go :: [(Int, [Hash Statement])] -> [(Int, [Hash Statement])]
    go positions =
      case positions of
        [] -> []
        p@(positionIndex, positionInserts) : rest
          | let adjustedIx = ix - positionIndex
          , 0 <= adjustedIx && adjustedIx < length positionInserts ->
              let
                (prefix, more) = splitAt adjustedIx positionInserts
              in
                case more of
                  [] -> error "impossible"
                  _ : suffix -> (positionIndex, prefix ++ val : suffix) : go rest
          | otherwise -> p : go rest

data Change :: * -> * where
  Replace :: { changeOld :: Hash a, changeNew :: Hash a } -> Change a
  Insert :: InsertPositions -> Change Block
deriving instance Show (Change a)

data Diff a where
  Leaf :: Change a -> Diff a
  Branch :: [Entry a] -> Diff a
deriving instance Show (Diff a)

emptyDiff :: Diff a
emptyDiff = Branch []

applyChange :: MonadStore m => Path a b -> Change b -> Hash a -> m (Maybe (Hash a))
applyChange p c h =
  case c of
    Replace _ newInner -> do
      m_res <- setH p (pure newInner) h
      pure $ Store.rootHash <$> m_res
    Insert (InsertPositions positions) ->
      Store.insertH p positions h

insert :: MonadStore m => Path a b -> Change b -> Diff a -> m (Diff a)
insert p c m =
  case p of
    Nil -> pure $ Leaf c
    Cons l p' ->
      case m of
        Branch ms -> Branch <$> entriesInsert l p' c ms
        Leaf cOuter ->
          case cOuter of
            Replace old newOuter -> do
              m_newOuter' <- applyChange p c newOuter
              pure $ case m_newOuter' of
                Nothing -> m
                Just newOuter' -> Leaf $ Replace old newOuter'
            Insert positions ->
              case l of
                Block_Index ix ->
                  case getPosition ix positions of
                    Nothing ->
                      -- there is an insert change, but there also needs to be other changes applied to existing
                      -- (non-inserted) block indices
                      error "TODO: handle this case"
                    Just statementh -> do
                      m_statementh' <- applyChange p' c statementh
                      pure $ case m_statementh' of
                        Nothing -> m
                        Just statementh' ->
                          let
                            positions' = setPosition ix statementh' positions
                          in
                            Leaf $ Insert positions'
  where
    entriesInsert :: MonadStore m => Level a b -> Path b c -> Change c -> [Entry a] -> m [Entry a]
    entriesInsert level path change entries =
      case entries of
        [] -> do
          diff <- insert path change emptyDiff
          pure [Entry level diff]
        entry@(Entry level' d) : rest ->
          case eqLevel level level' of
            Nothing -> (entry :) <$> entriesInsert level path change rest
            Just Refl ->
              (\m' -> Entry level m' : rest) <$>
              insert path change d

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
            Insert positions -> error "TODO: translate Insert into log entry" positions
        Branch entries ->
          entries >>= \(Entry level d') -> go (Path.snoc path level) d'
