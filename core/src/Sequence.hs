{-# language BangPatterns#-}
{-# language TypeFamilies #-}
{-# language InstanceSigs, ScopedTypeVariables #-}
module Sequence
  (IsSequence(..))
where

import Data.Function (on)
import qualified Data.List as List

class IsSequence a where
  type Item a :: *
  deleteAt :: Int -> a -> a
  insertAt :: Int -> Item a -> a -> a
  insertAll :: Item a -> [(Int, [Item a])] -> a -> a

instance IsSequence [a] where
  type Item [a] = a
  deleteAt ix = go ix
    where
      go !n xs =
        case xs of
          [] -> error $ "deleteAt: " <> show ix <> "out of bounds"
          x : rest ->
            if n == 0
            then rest
            else x : go (n-1) rest

  insertAt ix val = go ix
    where
      go !n xs =
        if n <= 0
        then val : xs
        else
          case xs of
            [] -> error $ "insertAt: " <> show ix <> "out of bounds"
            x : rest ->
              x : go (n-1) rest

  insertAll :: a -> [(Int, [a])] -> [a] -> [a]
  insertAll def inserts bs =
    -- support inserting at the tail of the list
    if maxIx >= length fullBs
    then resultsWithoutLast ++ maxEntries
    else resultsWithoutLast

    where
      (maxIx, maxEntries) = List.maximumBy (compare `on` fst) inserts

      -- if there are entries that occur after the end of the original list,
      -- pad the end of the list with an appropriate number of holes
      fullBs =
        bs ++
        replicate (maxIx - length bs) def

      -- intersperse the entries at the appropriate index
      resultsWithoutLast = do
        (ix, b) <- zip [0..] fullBs
        case lookup ix inserts of
          Nothing -> pure b
          Just moreBs -> moreBs ++ [b]
