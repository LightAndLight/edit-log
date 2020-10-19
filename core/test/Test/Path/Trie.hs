{-# language GADTs #-}
{-# language ScopedTypeVariables, TypeApplications #-}
module Test.Path.Trie (trieSpec) where

import Data.Functor.Const (Const(..))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import NodeType (nodeType)
import qualified NodeType
import Path (Path)
import Path.Trie (Trie)
import qualified Path.Trie as Trie

import Gen.Path (SomePath(..), genPath)
import Gen.Path.Trie (genTrieFor)

genVal :: Gen (Const Int b)
genVal = Const <$> Gen.int (Range.constant minBound maxBound)

trieSpec :: Spec
trieSpec =
  describe "trie" $ do
    it "forall path val trie. lookup path (insert path val trie) = Just val" . hedgehog $ do
      SomePath (path :: Path a b) <- forAll genPath
      val <- forAll genVal
      trie <- NodeType.showingNodeType (nodeType @a) (forAll $ genTrieFor (nodeType @a) (\_ -> genVal))
      Trie.lookup path (Trie.insert path val trie) === Just val
    it "forall path val val' trie. insert path val (insert path val' trie) = insert path val trie" . hedgehog $ do
      SomePath (path :: Path a b) <- forAll genPath
      val <- forAll genVal
      val' <- forAll genVal
      let nt = nodeType @a
      NodeType.showingNodeType nt $ do
        trie :: Trie a (Const Int) <- forAll $ genTrieFor (nodeType @a) (\_ -> genVal)
        NodeType.eqingNodeType nt $
          Trie.insert path val (Trie.insert path val' trie) ===
          Trie.insert path val trie
