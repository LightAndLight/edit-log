{-# language GADTs, KindSignatures #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language QuantifiedConstraints #-}
module Diff where

import Data.Foldable (foldlM, foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Type.Equality ((:~:)(..))

import Diff.SequenceDiff (SequenceDiff)
import qualified Diff.SequenceDiff as SequenceDiff
import Hash (Hash)
import qualified Log
import Node (Node(..))
import Path (Level(..), Path(..), eqLevel)
import qualified Path
import Sequence (IsSequence, Item)
import Store (MonadStore)
import qualified Store
import Syntax (Block(..), Statement(..))

data Entry a where
  Entry :: Level a b -> Diff b -> Entry a
deriving instance Show (Entry a)

getEntry :: Foldable f => Level a b -> f (Entry a) -> Maybe (Diff b)
getEntry level =
  foldr
    (\(Entry level' diff) rest ->
      case eqLevel level level' of
        Nothing -> rest
        Just Refl -> Just diff
    )
    Nothing

setEntry :: Level a b -> Diff b -> NonEmpty (Entry a) -> NonEmpty (Entry a)
setEntry level d entries =
  case NonEmpty.uncons entries of
    (entry@(Entry level' _), m_rest) ->
      case eqLevel level level' of
        Nothing ->
          case m_rest of
            Nothing -> pure entry
            Just rest -> NonEmpty.cons entry (setEntry level d rest)
        Just Refl ->
          case m_rest of
            Nothing -> pure $ Entry level d
            Just rest -> NonEmpty.cons (Entry level d) rest

data LeafChange :: * -> * where
  ReplaceLeaf :: { changeOld :: Hash a, changeNew :: Hash a } -> LeafChange a
  EditSequenceLeaf :: IsSequence a => SequenceDiff (Hash (Item a)) -> LeafChange a
deriving instance Show (LeafChange a)

data BranchChange :: * -> * where
  EditSequenceBranch :: IsSequence a => SequenceDiff (Hash (Item a)) -> BranchChange a
deriving instance Show (BranchChange a)

data Diff a where
  Empty :: Diff a
  Leaf :: LeafChange a -> Diff a
  Branch :: Maybe (BranchChange a) -> NonEmpty (Entry a) -> Diff a
deriving instance Show (Diff a)

emptyDiff :: Diff a
emptyDiff = Empty

applyChange :: MonadStore m => Path a b -> LeafChange b -> Hash a -> m (Maybe (Hash a))
applyChange p c h =
  case c of
    ReplaceLeaf _ newInner -> do
      m_res <- Store.setH p (pure newInner) h
      pure $ Store.rootHash <$> m_res
    EditSequenceLeaf changes ->
      Store.modifyH
        p
        (\hash -> do
           mNode <- Store.lookupNode hash
           case mNode of
             Nothing ->
               pure hash
             Just node ->
               case node of
                 NBlock statementHashes ->
                   case NonEmpty.nonEmpty $ SequenceDiff.apply changes (NonEmpty.toList statementHashes) of
                     Nothing ->
                       Store.addBlock . Block $ pure SHole
                     Just statementHashes' ->
                       Store.addNode $ NBlock statementHashes'
                 NArgs hs ->
                   Store.addNode . NArgs $ SequenceDiff.apply changes hs
                 NParams hs ->
                   Store.addNode . NParams $ SequenceDiff.apply changes hs
                 _ -> pure hash
        )
        h

editSequenceBranchChange ::
  MonadStore m =>
  SequenceDiff (Hash (Item a)) ->
  BranchChange a ->
  m (BranchChange a)
editSequenceBranchChange newChanges existingBranchChange =
  case existingBranchChange of
    EditSequenceBranch existingChanges ->
      pure . EditSequenceBranch $ SequenceDiff.after newChanges existingChanges

insert :: MonadStore m => Path a b -> LeafChange b -> Diff a -> m (Diff a)
insert p c currentDiff =
  case p of
    Nil ->
      case currentDiff of
        Empty -> pure $ Leaf c
        Branch m_branchChange ms ->
          case c of
            ReplaceLeaf{} -> pure $ Leaf c
            EditSequenceLeaf newChanges ->
              case m_branchChange of
                Nothing ->
                  pure $ Branch (Just $ EditSequenceBranch newChanges) ms
                Just existingBranchChange ->
                  (\branchChange' -> Branch (Just branchChange') ms) <$>
                  editSequenceBranchChange newChanges existingBranchChange
        Leaf{} -> pure $ Leaf c
    Cons l p' ->
      case currentDiff of
        Empty -> do
          entry <- Entry l <$> insert p' c emptyDiff
          pure $ Branch Nothing (pure entry)
        Branch branchChange entries ->
          case getEntry l entries of
            Nothing -> do
              entry <- Entry l <$> insert p' c emptyDiff
              pure $ Branch branchChange (NonEmpty.cons entry entries)
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
            EditSequenceLeaf changes ->
              case l of
                Block_Index ix ->
                  changeSequenceDiff l ix p' c currentDiff changes
                Args_Index ix ->
                  changeSequenceDiff l ix p' c currentDiff changes
                Params_Index ix ->
                  changeSequenceDiff l ix p' c currentDiff changes
                _ -> pure currentDiff

changeSequenceDiff ::
  (MonadStore m, IsSequence a) =>
  Level a (Item a) ->
  Int ->
  Path (Item a) b ->
  LeafChange b ->
  Diff a ->
  SequenceDiff (Hash (Item a)) ->
  m (Diff a)
changeSequenceDiff l ix path newChange currentDiff currentSequenceDiff =
  case SequenceDiff.valueAt ix currentSequenceDiff of
    SequenceDiff.Unknown -> do
      -- there is an insert change, but there also needs to be other changes applied to existing
      -- (non-inserted) args indices
      entry <- Entry l <$> insert path newChange emptyDiff
      pure $ Branch (Just $ EditSequenceBranch currentSequenceDiff) (pure entry)
    SequenceDiff.Known statementh -> do
      m_statementh' <- applyChange path newChange statementh
      pure $ case m_statementh' of
        Nothing -> currentDiff
        Just statementh' ->
          let
            newSequenceDiff = SequenceDiff.replace ix statementh' currentSequenceDiff
          in
            Leaf $ EditSequenceLeaf newSequenceDiff

traversal ::
  forall a b.
  Path a b ->
  forall f. Applicative f => (LeafChange b -> f (LeafChange b)) -> Diff a -> f (Diff a)
traversal p f m =
  case p of
    Nil ->
      case m of
        Empty -> pure m
        Leaf h -> Leaf <$> f h
        Branch{} -> pure m
    Cons l p' ->
      case m of
        Empty -> pure m
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
           insert path (EditSequenceLeaf $ SequenceDiff.insert ix new SequenceDiff.empty) acc
         Log.Delete path ix old -> error "TODO: translate Delete into diff entry" path ix old
    )
    emptyDiff

fromEditSequence ::
  IsSequence b =>
  Path a b ->
  SequenceDiff (Hash (Item b)) ->
  [Log.Entry a]
fromEditSequence path changes =
  -- the log entries appear in reverse, because inserting the very last 'position'
  -- first doesn't invalidate the indices of all the other inserts
  foldl'
    (\res (curIx, change) ->
       case change of
         SequenceDiff.Insert vals ->
           foldl (\vs v -> Log.Insert path curIx v : vs) res vals
         SequenceDiff.Replace vals ->
           error "TODO: translate Replace into log entries" vals
         SequenceDiff.Delete ->
           error "TODO: translate Delete into log entries"
    )
    []
    (SequenceDiff.toList changes)

fromDiff :: forall a. Diff a -> [Log.Entry a]
fromDiff = go Nil
  where
    go :: forall b. Path a b -> Diff b -> [Log.Entry a]
    go path d =
      case d of
        Empty -> []
        Leaf leafChange ->
          case leafChange of
            ReplaceLeaf old new -> [Log.Replace path old new]
            EditSequenceLeaf changes -> fromEditSequence path changes
        Branch branchChange entries ->
          foldMap
            (\change ->
               case change of
                 EditSequenceBranch changes -> fromEditSequence path changes
            )
            branchChange <>
          (NonEmpty.toList entries >>= \(Entry level d') -> go (Path.snoc path level) d')
