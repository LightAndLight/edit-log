module Main where

import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(..))

import Path (Path(..), Level(..))
import Syntax (Expr(..), BinOp(..))
import Versioned (replace, snapshot)
import Versioned.Pure (Versioned, newVersioned, runVersionedT)

main :: IO ()
main = do
  let
    v :: Versioned Expr
    v = newVersioned (BinOp Add (Int 1) (Int 2))

    Identity (vals, v') = runVersionedT v $ do
      v1 <- snapshot
      replace (Cons BinOp_Left Nil) (Int 3)
      v2 <- snapshot
      replace (Cons BinOp_Right Nil) (BinOp Mul (Int 3) (Int 4))
      v3 <- snapshot
      replace Nil EHole
      v4 <- snapshot
      pure [v1, v2, v3, v4]
  print v'
  traverse_ print vals
