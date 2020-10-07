{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
module Diff.SequenceDiff
  ( SequenceDiff
  , Change(..)
  , empty
  , size
  , insert
  , replace
  , delete
  , apply
  , toList
  )
where

import Control.Lens (over, mapped, _1)
import qualified Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

data Change a
  = Replace (NonEmpty a)
  | Insert (NonEmpty a)
  | Delete
  deriving (Eq, Show)

changeSize :: Change a -> Int
changeSize change =
  case change of
    Insert as -> length as
    Delete -> 1
    Replace as -> length as

newtype SequenceDiff a = SequenceDiff [(Int, Change a)]
  deriving (Eq, Show)

empty :: SequenceDiff a
empty = SequenceDiff mempty

size :: SequenceDiff a -> Int
size (SequenceDiff cs) = length cs

insert :: forall a. Int -> a -> SequenceDiff a -> SequenceDiff a
insert ix val (SequenceDiff cs) =
  SequenceDiff $ insertEntry ix cs
  where
    insertEntry :: Int -> [(Int, Change a)] -> [(Int, Change a)]
    insertEntry currentIx orderedCs =
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
                  -- sz = 1
                  --
                  -- k <= currentIx <= k + 1, therefore currentIx \in {k, k+1}
                  if currentIx == k
                  then (k, Replace $ pure val) : rest
                  else entry : insertEntry (currentIx - 1) rest
                Replace{} ->
                  -- sz = 1
                  --
                  -- k <= currentIx <= k + 1, therefore currentIx \in {k, k+1}
                  if currentIx == k
                  then (k, Replace $ pure val) : rest
                  else entry : insertEntry (currentIx - 1) rest
                Insert vals ->
                  -- The entry we're inserting should lie somewhere in vals.
                  --
                  -- Since we're iterating over entries in ascending order of
                  -- keys, we know this the canonical place for the insert
                  let
                    (prefix, suffix) = NonEmpty.splitAt (currentIx - k) vals
                  in
                    (k, Insert $ foldr NonEmpty.cons (val :| suffix) prefix) : over (mapped._1) (+1) rest
            else
              entry : insertEntry (currentIx - sz) rest
          else
            (currentIx, Insert $ pure val) : entry : rest

replace :: forall a. Int -> a -> SequenceDiff a -> SequenceDiff a
replace ix val (SequenceDiff cs) = SequenceDiff $ go ix cs
  where
    go currentIndex entries =
      case entries of
        [] -> [(currentIndex, Replace $ pure val)]
        entry@(k, change) : rest ->
          let sz = changeSize change in
          if k <= currentIndex
          then
            if currentIndex <= k + sz
            then
              case change of
                Delete ->
                  error "impossible"
                Replace vals ->
                  if currentIndex == k
                  then
                    let
                      (prefix, suffix) = NonEmpty.splitAt (currentIndex - k) vals
                    in
                      (k, Replace $ foldr NonEmpty.cons (val :| tail suffix) prefix) : rest
                  else
                    entry : go (currentIndex - 1) rest
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

delete :: Show a => Int -> SequenceDiff a -> SequenceDiff a
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
                  error "impossible"
                Replace vals ->
                  if currentIx < k + sz
                  then
                    let
                      (prefix, suffix) = NonEmpty.splitAt (currentIx - k) vals
                      rest' = over (mapped._1) (subtract 1) rest
                    in
                      case NonEmpty.nonEmpty (prefix <> tail suffix) of
                        Nothing -> rest'
                        Just vals' ->
                          (k, Insert vals') : rest'
                  else
                    entry : go (currentIx - sz) rest
                Insert vals ->
                  let rest' = over (mapped._1) (subtract 1) rest in
                  if currentIx < k + sz
                  then
                    let
                      (prefix, suffix) = NonEmpty.splitAt (currentIx - k) vals
                    in
                      case NonEmpty.nonEmpty (prefix <> tail suffix) of
                        Nothing -> rest'
                        Just vals' ->
                          (k, Insert vals') : rest'
                  else
                    case rest of
                      x : _ | fst x == currentIx ->
                        entry : go (currentIx - sz) rest
                      _ ->
                        (k, Replace vals) : rest'
            else
              entry : go (currentIx - sz) rest
          else
            (currentIx, Delete) : entry : rest

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
