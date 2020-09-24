module Main where

import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(..))

import qualified Diff
import Log (Time, Entry)
import qualified Log
import Path (Path(..), Level(..))
import Syntax (Expr(..), BinOp(..))
import Versioned (replace, snapshot)
import Versioned.Pure (Versioned, newVersioned, runVersionedT, debugLog)
import Session (Session, newSession, undo, redo)
import Session.Pure (runSessionT)

main :: IO ()
main = do
  let
    v :: Versioned Expr
    v = newVersioned (BinOp Add (Int 1) (Int 2))

    s :: Session (Time, Entry Expr)
    s = newSession

    Identity ((diff, vals), v', s') = runSessionT v s $ do
      v1 <- snapshot
      replace (Cons BinOp_Left Nil) (Int 3)
      v2 <- snapshot
      replace (Cons BinOp_Right Nil) (BinOp Mul (Int 3) (Int 4))
      v3 <- snapshot
      replace Nil EHole
      v4 <- snapshot
      undo
      v5 <- snapshot
      undo
      v6 <- snapshot
      redo
      v7 <- snapshot
      es <- fmap snd <$> Log.getEntries
      diff <- Diff.toDiff es
      pure (diff, [v1, v2, v3, v4, v5, v6, v7])
  print v'
  print $ debugLog v'
  print s'
  print diff
  traverse_ print vals
