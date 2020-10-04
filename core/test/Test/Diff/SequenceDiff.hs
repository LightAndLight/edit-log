{-# language BangPatterns #-}
{-# language OverloadedLists #-}
module Test.Diff.SequenceDiff (sequenceDiffSpec) where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

import Diff.SequenceDiff (SequenceDiff)
import qualified Diff.SequenceDiff as SequenceDiff

genVal :: Gen String
genVal = Gen.string (Range.constant 0 10) Gen.alphaNum

genSequenceDiff :: Gen a -> Gen (SequenceDiff a)
genSequenceDiff genA = Gen.int (Range.constant 0 100) >>= go SequenceDiff.empty
  where
    go value !count =
      if count > 0
      then do
        f <-
          Gen.choice
          [ SequenceDiff.insert <$>
            Gen.int (Range.constant 0 $ SequenceDiff.size value) <*>
            genA
          ]
        go (f value) (count-1)
      else pure value

insert :: Int -> a -> [a] -> [a]
insert ix val xs = let (prefix, suffix) = splitAt ix xs in prefix ++ val : suffix

sequenceDiffSpec :: Spec
sequenceDiffSpec =
  describe "SequenceDiff" $ do
    it "toList (insert 0 () empty) = [(0, Insert [()])]" $
      SequenceDiff.toList (SequenceDiff.insert 0 () SequenceDiff.empty) `shouldBe`
      [(0, SequenceDiff.Insert $ pure ())]

    it "toList (insert 0 \"a\" $ insert 0 \"b\" empty) = [(0, Insert [\"a\", \"b\"])]" $
      SequenceDiff.toList (SequenceDiff.insert 0 "a" $ SequenceDiff.insert 0 "b" SequenceDiff.empty) `shouldBe`
      [(0, SequenceDiff.Insert ["a", "b"])]

    it "apply (insert 0 () empty) [] = [()]" $
      SequenceDiff.apply (SequenceDiff.insert 0 () SequenceDiff.empty) [] `shouldBe`
      [()]

    it "forall xs. apply empty xs = xs" . hedgehog $ do
      xs <- forAll $ Gen.list (Range.constant 0 100) genVal
      SequenceDiff.apply SequenceDiff.empty xs === xs

    it "insert 1 \"d\" [\"a\", \"b\", \"c\"] = [\"a\", \"d\", \"b\", \"c\"]" $ do
      insert 1 "d" ["a", "b", "c"] `shouldBe` ["a", "d", "b", "c"]

    it "toList (insert 1 \"d\" empty) = [(1, Insert [\"d\"])]" $ do
      SequenceDiff.toList (SequenceDiff.insert 1 "d" SequenceDiff.empty) `shouldBe`
        [(1, SequenceDiff.Insert ["d"])]

    it "apply (insert 1 \"d\" empty) [\"a\", \"b\", \"c\"] = [\"a\", \"d\", \"b\", \"c\"]" $ do
      SequenceDiff.apply (SequenceDiff.insert 1 "d" SequenceDiff.empty) ["a", "b", "c"] `shouldBe`
        ["a", "d", "b", "c"]

    it "insert 1 \"d\" (apply empty [\"a\", \"b\", \"c\"]) = apply (insert 1 \"d\" empty) [\"a\", \"b\", \"c\"]" $ do
      insert 1 "d" (SequenceDiff.apply SequenceDiff.empty ["a", "b", "c"]) `shouldBe`
        SequenceDiff.apply (SequenceDiff.insert 1 "d" SequenceDiff.empty) ["a", "b", "c"]

    it "toList (insert 0 \"b\" $ insert 1 \"a\" empty) = [(0, Insert [\"b\"]), (1, Insert [\"a\"])]" $ do
      SequenceDiff.toList (SequenceDiff.insert 0 "b" $ SequenceDiff.insert 1 "a" SequenceDiff.empty) `shouldBe`
        [(0, SequenceDiff.Insert ["b"]), (1, SequenceDiff.Insert ["a"])]

    it "apply (insert 0 \"b\" $ insert 1 \"a\" empty) [\"x\", \"y\"] = [\"b\", \"x\", \"a\", \"y\"]" $ do
      SequenceDiff.apply (SequenceDiff.insert 0 "b" $ SequenceDiff.insert 1 "a" SequenceDiff.empty) ["x", "y"] `shouldBe`
        ["b", "x", "a", "y"]

    it "toList (insert 1 \"b\" $ insert 0 \"a\" empty) = [(0, Insert [\"a\", \"b\"])]" $ do
      SequenceDiff.toList (SequenceDiff.insert 1 "b" $ SequenceDiff.insert 0 "a" SequenceDiff.empty) `shouldBe`
        [(0, SequenceDiff.Insert ["a", "b"])]

    it "toList (insert 2 \"b\" $ insert 0 \"a\" empty) = [(0, Insert [\"a\"]), (1, Insert [\"b\"])]" $ do
      SequenceDiff.toList (SequenceDiff.insert 2 "b" $ SequenceDiff.insert 0 "a" SequenceDiff.empty) `shouldBe`
        [(0, SequenceDiff.Insert ["a"]), (1, SequenceDiff.Insert ["b"])]

    it "forall ix val cs xs. insert ix val (apply cs xs) = apply (insert ix val cs) xs" . hedgehog $ do
      xs <- forAll $ Gen.list (Range.constant 0 100) genVal
      ix <- forAll $ Gen.int (Range.constant 0 $ length xs)
      val <- forAll genVal
      cs <- forAll $ genSequenceDiff genVal
      insert ix val (SequenceDiff.apply cs xs) ===
        SequenceDiff.apply (SequenceDiff.insert ix val cs) xs
