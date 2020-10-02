{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
module Diff.SequenceDiff
  ( SequenceDiff
  , insert
  {-
  , delete
  , apply
  -}
  , toList
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

data Change a
  = Insert (NonEmpty a)
  | Delete

changeSize :: Change a -> Int
changeSize change =
  case change of
    Insert as -> length as
    Delete -> 1

newtype SequenceDiff a = SequenceDiff (IntMap (Change a))

insert :: forall a. Int -> a -> SequenceDiff a -> SequenceDiff a
insert ix val (SequenceDiff cs) =
  SequenceDiff $ IntMap.insert ix' val' cs
  where
    computeEntry :: Int -> Change a -> [(Int, Change a)] -> (Int, Change a)
    computeEntry accIx accVal orderedCs =
      case orderedCs of
        [] -> (accIx, accVal)
        (k, change) : rest ->
          let sz = changeSize change in
          if k <= accIx
          then
            if accIx < k + sz
            then
              case change of
                Delete ->
                  -- sz = 1
                  --
                  -- k <= ix < k + 1, therefore ix = k
                  --
                  -- increment.
                  computeEntry (accIx + sz) accVal rest
                Insert vals ->
                  -- The entry we're inserting should lie somewhere in vals.
                  --
                  -- Since we're iterating over entries in ascending order of
                  -- keys, we know this the canonical place for the insert
                  let
                    (prefix, suffix) = NonEmpty.splitAt (k + sz - accIx) vals
                  in
                    (accIx, Insert $ foldr NonEmpty.cons (val :| suffix) prefix)
            else
              computeEntry (accIx + sz) accVal rest
          else
            (accIx, accVal)

    (ix', val') = computeEntry ix (Insert $ pure val) (IntMap.toAscList cs)

{-

delete :: Int -> SequenceDiff a -> SequenceDiff a
delete ix (SequenceDiff cs) =
  SequenceDiff $
  _

apply :: SequenceDiff a -> [a] -> [a]
apply (SequenceDiff a) xs =
  _

-}

toList :: SequenceDiff a -> [(Int, Change a)]
toList (SequenceDiff cs) = IntMap.toList cs
