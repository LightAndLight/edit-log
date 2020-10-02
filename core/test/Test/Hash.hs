module Test.Hash (hashSpec) where

import Data.Maybe (isJust)
import Data.Some (Some(..))
import Hedgehog
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Gen.Hash (genHash, genEqualHash, genEqualHashes)

import Hash (Hash, eqHash)

equalHashes :: Hash a -> Hash b -> PropertyT IO ()
equalHashes t1 t2 = diff t1 (\a b -> isJust $ eqHash a b) t2

hashSpec :: Spec
hashSpec =
  describe "hash" $ do
    describe "generators" $ do
      it "genEqualHash" . hedgehog $ do
        Some h <- forAll genHash
        Some h' <- forAll $ genEqualHash (Some h)
        equalHashes h h'
      it "genEqualHashes" . hedgehog $ do
        (Some h, Some h') <- forAll genEqualHashes
        equalHashes h h'
    describe "eqHash" $ do
      it "reflexive" . hedgehog $ do
        Some h <- forAll genHash
        equalHashes h h
      it "transitive" . hedgehog $ do
        Some h1 <- forAll genHash
        Some h2 <- forAll $ genEqualHash (Some h1)
        Some h3 <- forAll $ genEqualHash (Some h2)
        equalHashes h1 h3
      it "symmetric" . hedgehog $ do
        (Some h1, Some h2) <- forAll genEqualHashes
        equalHashes h2 h1
