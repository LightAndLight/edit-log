{-# language GADTs, KindSignatures #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
module Diff where

import Data.Foldable (foldlM, foldl')
import Data.Function (on)
import qualified Data.List as List
import Data.Type.Equality ((:~:)(..))

import qualified Log
import Node (Hash)
import Path (Level(..), Path(..), eqLevel)
import qualified Path
import Store (MonadStore, setH)
import qualified Store
import Syntax (Block, Statement(..))

data Entry a where
  Entry :: Level a b -> Diff b -> Entry a
deriving instance Show (Entry a)

getEntry :: Level a b -> [Entry a] -> Maybe (Diff b)
getEntry level entries =
  case entries of
    [] -> Nothing
    Entry level' diff : rest ->
      case eqLevel level level' of
        Nothing -> getEntry level rest
        Just Refl -> Just diff

setEntry :: Level a b -> Diff b -> [Entry a] -> [Entry a]
setEntry level d entries =
  case entries of
    [] -> [Entry level d]
    Entry level' diff : rest ->
      case eqLevel level level' of
        Nothing -> Entry level' diff : setEntry level d rest
        Just Refl -> Entry level' d : rest

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

-- replace existing entry
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

-- insert entry, or replace if it already exists
insertPosition ::
  forall m.
  MonadStore m =>
  Int ->
  Hash Statement ->
  InsertPositions ->
  m InsertPositions
insertPosition ix val (InsertPositions ps) = InsertPositions <$> go ps
  where
    go :: [(Int, [Hash Statement])] -> m [(Int, [Hash Statement])]
    go positions =
      case positions of
        [] -> pure [(ix, [val])]
        p@(positionIndex, positionInserts) : rest
          | let adjustedIx = ix - positionIndex
          , 0 <= adjustedIx ->
            case compare adjustedIx (length positionInserts) of
              LT ->
                let
                  (prefix, more) = splitAt adjustedIx positionInserts
                in
                  case more of
                    [] -> error "impossible"
                    _ : suffix -> pure $ (positionIndex, prefix ++ val : suffix) : rest
              _ -> do
                h <- Store.addStatement SHole
                let holes = replicate (adjustedIx - length positionInserts) h
                pure $ (positionIndex, positionInserts ++ holes ++ [val]) : rest
          | otherwise -> (p :) <$> go rest

data LeafChange :: * -> * where
  ReplaceLeaf :: { changeOld :: Hash a, changeNew :: Hash a } -> LeafChange a
  InsertLeaf :: InsertPositions -> LeafChange Block
deriving instance Show (LeafChange a)

data BranchChange :: * -> * where
  InsertBranch :: InsertPositions -> BranchChange Block
deriving instance Show (BranchChange a)

data Diff a where
  Leaf :: LeafChange a -> Diff a
  Branch :: Maybe (BranchChange a) -> [Entry a] -> Diff a
deriving instance Show (Diff a)

emptyDiff :: Diff a
emptyDiff = Branch Nothing []

applyChange :: MonadStore m => Path a b -> LeafChange b -> Hash a -> m (Maybe (Hash a))
applyChange p c h =
  case c of
    ReplaceLeaf _ newInner -> do
      m_res <- setH p (pure newInner) h
      pure $ Store.rootHash <$> m_res
    InsertLeaf (InsertPositions positions) ->
      Store.insertH p positions h

insertPositionsBranchChange ::
  MonadStore m =>
  InsertPositions ->
  BranchChange Block ->
  m (BranchChange Block)
insertPositionsBranchChange (InsertPositions positions) change =
  case change of
    InsertBranch positions' ->
      InsertBranch <$>
      foldlM
        (\acc (ix, h) -> insertPosition ix h acc)
        positions'
        (positions >>= \(ix, ps) -> zip [ix..] ps)

insert :: MonadStore m => Path a b -> LeafChange b -> Diff a -> m (Diff a)
insert p c currentDiff =
  case p of
    Nil ->
      case currentDiff of
        Branch m_branchChange ms ->
          case c of
            ReplaceLeaf{} -> pure $ Leaf c
            InsertLeaf positions ->
              case m_branchChange of
                Nothing ->
                  pure $ Branch (Just $ InsertBranch positions) ms
                Just branchChange ->
                  (\branchChange' -> Branch (Just branchChange') ms) <$>
                  insertPositionsBranchChange positions branchChange
        Leaf{} -> pure $ Leaf c
    Cons l p' ->
      case currentDiff of
        Branch branchChange entries ->
          case getEntry l entries of
            Nothing -> do
              entry <- Entry l <$> insert p' c emptyDiff
              pure $ Branch branchChange (entry : entries)
            Just m' -> do
              m'' <- insert p' c m'
              let entries' = setEntry l m'' entries
              pure $ Branch branchChange entries'
        Leaf cOuter ->
          case cOuter of
            ReplaceLeaf old newOuter -> do
              m_newOuter' <- applyChange p c newOuter
              pure $ case m_newOuter' of
                Nothing -> currentDiff
                Just newOuter' -> Leaf $ ReplaceLeaf old newOuter'
            InsertLeaf positions ->
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
                        Nothing -> currentDiff
                        Just statementh' ->
                          let
                            positions' = setPosition ix statementh' positions
                          in
                            Leaf $ InsertLeaf positions'

traversal ::
  forall a b.
  Path a b ->
  forall f. Applicative f => (LeafChange b -> f (LeafChange b)) -> Diff a -> f (Diff a)
traversal p f m =
  case p of
    Nil ->
      case m of
        Leaf h -> Leaf <$> f h
        Branch{} -> pure m
    Cons l p' ->
      case m of
        Leaf{} -> pure m
        Branch branchChange ms ->
          Branch branchChange <$>
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
           insert path (ReplaceLeaf old new) acc
         Log.Insert path ix new ->
           insert path (InsertLeaf $ InsertPositions [(ix, [new])]) acc
         Log.Delete path ix old -> error "TODO: translate Delete into diff entry" path ix old
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
            ReplaceLeaf old new -> [Log.Replace path old new]
            InsertLeaf (InsertPositions positions) ->
              -- the log entries appear in reverse, because inserting the very last 'position'
              -- first doesn't invalidate the indices of all the other inserts
              foldl'
                (\res (curIx, vals) ->
                  foldl (\vs v -> Log.Insert path curIx v : vs) res vals
                )
                []
                (List.sortBy (compare `on` fst) positions)
        Branch branchChange entries ->
          foldMap
            (\change ->
               case change of
                 InsertBranch positions -> error "TODO: translate Insert into log entry" positions
            )
            branchChange ++
          (entries >>= \(Entry level d') -> go (Path.snoc path level) d')
