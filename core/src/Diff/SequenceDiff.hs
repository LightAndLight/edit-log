{-# language BangPatterns #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language ScopedTypeVariables #-}
module Diff.SequenceDiff
  ( SequenceDiff
  , Change(..)
  , empty
  , after
  , size
  , insert
  , replace
  , delete
  , apply
  , ValueAt(..)
  , valueAt
  , toList
  )
where

import qualified Data.IntMap as IntMap
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

data Change a
  = Replace (NonEmpty a)
  | Insert (NonEmpty a)
  | Delete
  deriving (Eq, Show, Functor, Foldable, Traversable)

changeSize :: Change a -> Int
changeSize change =
  case change of
    Insert vals -> length vals
    Delete -> -1
    Replace vals -> length vals - 1

newtype SequenceDiff a = SequenceDiff [(Int, Change a)]
  deriving (Eq, Show)

empty :: SequenceDiff a
empty = SequenceDiff mempty

size :: SequenceDiff a -> Int
size (SequenceDiff cs) = length cs

insert :: forall a. Int -> a -> SequenceDiff a -> SequenceDiff a
insert ix val (SequenceDiff cs) =
  SequenceDiff $ go ix cs
  where
    go :: Int -> [(Int, Change a)] -> [(Int, Change a)]
    go currentIx orderedCs =
      case orderedCs of
        [] -> [(currentIx, Insert $ pure val)]
        entry@(k, change) : rest ->
          let sz = changeSize change in
          if k <= currentIx
          then
            if currentIx <= k + sz
            then
              case change of
                Delete ->
                  entry : go (currentIx + 1) rest
                Replace vals ->
                  let
                    (prefix, suffix) = NonEmpty.splitAt (currentIx - k) vals
                  in
                    (k, Replace $ foldr NonEmpty.cons (val :| suffix) prefix) : rest
                Insert vals ->
                  -- The entry we're inserting should lie somewhere in vals.
                  --
                  -- Since we're iterating over entries in ascending order of
                  -- keys, we know this the canonical place for the insert
                  let
                    (prefix, suffix) = NonEmpty.splitAt (currentIx - k) vals
                  in
                    (k, Insert $ foldr NonEmpty.cons (val :| suffix) prefix) : rest
            else
              entry : go (currentIx - sz) rest
          else
            (currentIx, Insert $ pure val) : entry : rest

replace :: forall a. Int -> a -> SequenceDiff a -> SequenceDiff a
replace ix val (SequenceDiff cs) = SequenceDiff $ go ix cs
  where
    go currentIndex entries =
      case entries of
        [] -> [(currentIndex, Replace $ pure val)]
        entry@(k, change) : rest ->
          --  1
          let sz = changeSize change in
          if k <= currentIndex
          then
            if currentIndex <= k + sz
            then
              case change of
                Delete ->
                  entry : go (currentIndex + 1) rest
                Replace vals ->
                  let
                    (prefix, suffix) = NonEmpty.splitAt (currentIndex - k) vals
                  in
                    (k, Replace $ foldr NonEmpty.cons (val :| tail suffix) prefix) : rest
                Insert vals ->
                  if currentIndex < k + sz
                  then
                    let
                      (prefix, suffix) = NonEmpty.splitAt (currentIndex - k) vals
                    in
                      (k, Insert $ foldr NonEmpty.cons (val :| tail suffix) prefix) : rest
                  else
                    (k, Replace $ vals <> pure val) : rest
            else
              entry : go (currentIndex - sz) rest
          else
            (currentIndex, Replace $ pure val) : entry : rest

delete :: Int -> SequenceDiff a -> SequenceDiff a
delete ix (SequenceDiff cs) =
  SequenceDiff $ go ix cs
  where
    go currentIx entries =
      case entries of
        [] -> [(currentIx, Delete)]
        entry@(k, change) : rest ->
          let sz = changeSize change in
          if k <= currentIx
          then
            if currentIx <= k + sz
            then
              case change of
                Delete ->
                  entry : go (currentIx + 1) rest
                Replace vals ->
                  let
                    (prefix, suffix) = NonEmpty.splitAt (currentIx - k) vals
                  in
                    case NonEmpty.nonEmpty (prefix <> tail suffix) of
                      Nothing -> (k, Delete) : rest
                      Just vals' ->
                        (k, Replace vals') : rest
                Insert vals ->
                  if currentIx < k + sz
                  then
                    let
                      (prefix, suffix) = NonEmpty.splitAt (currentIx - k) vals
                    in
                      case NonEmpty.nonEmpty (prefix <> tail suffix) of
                        Nothing -> rest
                        Just vals' ->
                          (k, Insert vals') : rest
                  else
                    (k, Replace vals) : rest
            else
              entry : go (currentIx - sz) rest
          else
            (currentIx, Delete) : entry : rest

after :: SequenceDiff a -> SequenceDiff a -> SequenceDiff a
after newChanges existingChanges =
  snd $
  foldl'
    (\(ixShift, acc) (ix, change) ->
       let sz = changeSize change in
       ( ixShift + sz
       , case change of
           Insert vals ->
             foldl'
               (\acc' val -> insert (ix+ixShift) val acc')
               acc
               (reverse $ NonEmpty.toList vals)
           Replace vals ->
             let slav = reverse . NonEmpty.toList $ vals in
             foldl'
               (\acc' val -> insert (ix+ixShift) val acc')
               (replace (ix+ixShift) (head slav) acc)
               (tail slav)
           Delete -> delete (ix+ixShift) acc
       )
    )
    (0, existingChanges)
    (toList newChanges)

apply :: SequenceDiff a -> [a] -> [a]
apply (SequenceDiff cs) xs =
  let
    xsLen = length xs
  in
    foldr
      (\(ix, x) rest ->
        case IntMap.lookup ix csMap of
          Nothing -> x : rest
          Just change ->
            case change of
              Insert vals -> NonEmpty.toList vals ++ x : rest
              Delete -> rest
              Replace vals -> foldr (:) rest vals
      )
      (case IntMap.lookup xsLen csMap of
         Nothing -> []
         Just change ->
           case change of
             Insert vals -> NonEmpty.toList vals
             Delete -> error $ "bounds error: " <> show xsLen
             Replace{} -> error "bounds error"
      )
      (zip [0..] xs)
  where
    csMap = IntMap.fromList cs


toList :: SequenceDiff a -> [(Int, Change a)]
toList (SequenceDiff cs) = cs

data ValueAt a
  = Known a
  | Unknown
  deriving (Eq, Show)

valueAt :: Int -> SequenceDiff a -> ValueAt a
valueAt ix = go ix . toList
  where
    go !currentIx cs =
      case cs of
        [] -> Unknown
        (k, change) : rest ->
          let sz = changeSize change in
          if k <= currentIx
          then
            if currentIx <= k + sz
            then
              case change of
                Insert vals ->
                  if currentIx < k + sz
                  then
                    let
                      (_, suffix) =
                        NonEmpty.splitAt (currentIx - k) vals
                    in
                      Known $ head suffix
                  else
                    go (currentIx - sz) rest
                Replace vals ->
                  let
                    (_, suffix) =
                      NonEmpty.splitAt (currentIx - k) vals
                  in
                    Known $ head suffix
                Delete ->
                  Unknown
            else
              go (currentIx - sz) rest
          else
            go (currentIx - sz) rest
