{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
module Diff.SequenceDiff
  ( SequenceDiff
  , Change(..)
  , empty
  , size
  , insert
  {-
  , delete
  -}
  , apply
  , toList
  )
where

import qualified Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

data Change a
  = Insert (NonEmpty a)
  | Delete
  deriving (Eq, Show)

changeSize :: Change a -> Int
changeSize change =
  case change of
    Insert as -> length as
    Delete -> 1

newtype SequenceDiff a = SequenceDiff [Change a]
  deriving (Eq, Show)

empty :: SequenceDiff a
empty = SequenceDiff mempty

size :: SequenceDiff a -> Int
size (SequenceDiff cs) = length cs

insert :: forall a. Int -> a -> SequenceDiff a -> SequenceDiff a
insert ix val (SequenceDiff cs) =
  SequenceDiff $ insertEntry (zip [0..] cs)
  where
    insertEntry :: [(Int, Change a)] -> [Change a]
    insertEntry orderedCs =
      case orderedCs of
        [] -> [Insert $ pure val]
        (k, change) : rest ->
          let sz = changeSize change in
          if k <= ix
          then -- here
            if ix < k + sz
            then
              case change of
                Delete ->
                  -- sz = 1
                  --
                  -- k <= ix < k + 1, therefore ix = k
                  --
                  -- increment.
                  change : insertEntry rest
                Insert vals ->
                  -- The entry we're inserting should lie somewhere in vals.
                  --
                  -- Since we're iterating over entries in ascending order of
                  -- keys, we know this the canonical place for the insert
                  let
                    (prefix, suffix) = NonEmpty.splitAt (ix - k) vals
                  in
                    Insert (foldr NonEmpty.cons (val :| suffix) prefix) : fmap snd rest
            else
              change : insertEntry rest
          else
            change : insertEntry rest

{-

delete :: Int -> SequenceDiff a -> SequenceDiff a
delete ix (SequenceDiff cs) =
  SequenceDiff $
  _

-}

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
              Insert vals -> NonEmpty.toList vals ++ rest
              Delete -> rest
      )
      (case IntMap.lookup xsLen csMap of
         Nothing -> []
         Just change ->
           case change of
             Insert vals -> NonEmpty.toList vals
             Delete -> error "bounds error"
      )
      (zip [0..] xs)
  where
    csMap = IntMap.fromList $ zip [0..] cs


toList :: SequenceDiff a -> [(Int, Change a)]
toList (SequenceDiff cs) = zip [0..] cs
