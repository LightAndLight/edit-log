module Diff.DeleteSet
  ( DeleteSet
  , empty
  , insert
  , toList
  )
where

newtype DeleteSet
  = DeleteSet [Int]
  deriving Show

empty :: DeleteSet
empty = DeleteSet mempty

insert :: Int -> DeleteSet -> DeleteSet
insert ix (DeleteSet ds) =
  DeleteSet $ go (ix + length (filter (<= ix) ds))
  where
    go i =
      if i `elem` ds
      then go (i+1)
      else i : ds

-- no ordering guarantee is made
toList :: DeleteSet -> [Int]
toList (DeleteSet ds) = ds
