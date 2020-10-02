{-# language BangPatterns #-}
module Diff.DeleteSet
  ( DeleteSet
  , empty
  , insert
  , member
  , toList
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

newtype DeleteSet a
  = DeleteSet (IntMap a)
  deriving Show

empty :: DeleteSet a
empty = DeleteSet mempty

insert :: Int -> a -> DeleteSet a -> DeleteSet a
insert ix val (DeleteSet ds) =
  DeleteSet $ go (ix + IntMap.foldlWithKey' (\acc k _ -> if k <= ix then acc + 1 else acc) 0 ds)
  where
    go !i =
      if IntMap.member i ds
      then go (i+1)
      else IntMap.insert i val ds

member :: Int -> DeleteSet a -> Bool
member ix (DeleteSet ds) = IntMap.member ix ds

-- no ordering guarantee is made
toList :: DeleteSet a -> [(Int, a)]
toList (DeleteSet ds) = IntMap.toList ds
