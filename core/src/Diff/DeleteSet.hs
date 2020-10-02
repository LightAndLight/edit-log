{-# language BangPatterns #-}
module Diff.DeleteSet
  ( DeleteSet
  , empty
  , insert
  , toList
  )
where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

newtype DeleteSet
  = DeleteSet IntSet
  deriving Show

empty :: DeleteSet
empty = DeleteSet mempty

insert :: Int -> DeleteSet -> DeleteSet
insert ix (DeleteSet ds) =
  DeleteSet $ go (ix + IntSet.foldl' (\acc k -> if k <= ix then acc + 1 else acc) 0 ds)
  where
    go !i =
      if IntSet.member i ds
      then go (i+1)
      else IntSet.insert i ds

-- no ordering guarantee is made
toList :: DeleteSet -> [Int]
toList (DeleteSet ds) = IntSet.toList ds
