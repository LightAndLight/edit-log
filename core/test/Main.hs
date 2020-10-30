module Main where

import Test.Hspec

import Test.Diff.SequenceDiff (sequenceDiffSpec)
import Test.Hash (hashSpec)
import Test.NodeType (nodeTypeSpec)
import Test.Path.Trie (trieSpec)
import Test.Store.Pure (storePureSpec)

main :: IO ()
main =
  hspec $ do
    hashSpec
    nodeTypeSpec
    storePureSpec
    sequenceDiffSpec
    trieSpec
