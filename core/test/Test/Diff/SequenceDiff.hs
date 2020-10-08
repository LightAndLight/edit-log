{-# language BangPatterns #-}
{-# language OverloadedLists #-}
{-# language RankNTypes #-}
module Test.Diff.SequenceDiff (sequenceDiffSpec) where

import Control.Monad.IO.Class
import qualified Data.List as List
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

data Trace a
  = Insert Int a (SequenceDiff a) [a]
  | Replace Int a (SequenceDiff a) [a]
  | Delete Int (SequenceDiff a) [a]
  deriving Show

genValidSequenceDiff ::
  Show a =>
  (forall x. Gen x -> Gen [x]) ->
  Gen a ->
  Gen ([a], [Trace a], SequenceDiff a, [a])
genValidSequenceDiff genS genA = do
  n <- Gen.int $ Range.constant 0 100
  initialSequence <- genS genA
  (trc, finalDiff, finalSequence) <- go n id SequenceDiff.empty initialSequence
  pure (initialSequence, trc [], finalDiff, finalSequence)
  where
    go 0 t d s = pure (t, d, s)
    go n t d s = do
      let len = length s
      (t', d', s') <-
        Gen.choice $
        [ do -- insert
            ix <- Gen.int $ Range.constant 0 len
            val <- genA
            pure (t . (:) (Insert ix val d s), SequenceDiff.insert ix val d, insert ix val s)
        ] <>
        (if len > 0
         then
           [ do -- replace
               ix <- Gen.int $ Range.constant 0 (len - 1)
               val <- genA
               pure (t . (:) (Replace ix val d s), SequenceDiff.replace ix val d, replace ix val s)
           , do -- delete
               ix <- Gen.int $ Range.constant 0 (len - 1)
               pure (t . (:) (Delete ix d s), SequenceDiff.delete ix d, delete ix s)
           ]
         else
           []
        )
      go (n-1) t' d' s'

insert :: Int -> a -> [a] -> [a]
insert ix val xs = let (prefix, suffix) = splitAt ix xs in prefix ++ val : suffix

delete :: Int -> [a] -> [a]
delete ix xs = let (prefix, suffix) = splitAt ix xs in prefix ++ tail suffix

replace :: Int -> a -> [a] -> [a]
replace ix val xs = let (prefix, suffix) = splitAt ix xs in prefix ++ val : tail suffix

sequenceDiffSpec :: Spec
sequenceDiffSpec =
  describe "SequenceDiff" $ do
    describe "generators" $ do
      it "valid sequence diff" . hedgehog $ do
        (initial, t, d, final) <-
          forAll $
          genValidSequenceDiff
            (Gen.list $ Range.constant 0 100)
            (Gen.string (Range.constant 0 100) Gen.alphaNum)

        liftIO $ print (initial, t, d, final)

        let dListIxs = fst <$> SequenceDiff.toList d

        List.nub dListIxs === dListIxs
        SequenceDiff.apply d initial === final
    describe "unit tests" $ do
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

      it "apply (delete 0 $ insert 0 \"a\" empty) [\"x\"] = [\"x\"]" $ do
        SequenceDiff.apply (SequenceDiff.delete 0 $ SequenceDiff.insert 0 "a" SequenceDiff.empty) ["x"] `shouldBe`
          ["x"]

      it "toList (delete 1 $ insert 0 \"a\" empty) = [(0, Replace [\"a\"])]" $ do
        SequenceDiff.toList (SequenceDiff.delete 1 $ SequenceDiff.insert 0 "a" SequenceDiff.empty) `shouldBe`
          [(0, SequenceDiff.Replace ["a"])]

      it "apply (delete 1 $ insert 0 \"a\" empty) [\"x\"] = [\"a\"]" $ do
        SequenceDiff.apply (SequenceDiff.delete 1 $ SequenceDiff.insert 0 "a" SequenceDiff.empty) ["x"] `shouldBe`
          ["a"]

      it "toList (delete 3 $ insert 0 \"a\" $ insert 0 \"b\" $ insert 0 \"c\" empty) = [(0, Replace [\"a\", \"b\", \"c\"])]" $ do
        SequenceDiff.toList
          (SequenceDiff.delete 3 $
           SequenceDiff.insert 0 "a" $
           SequenceDiff.insert 0 "b" $
           SequenceDiff.insert 0 "c" $
           SequenceDiff.empty
          ) `shouldBe`
          [(0, SequenceDiff.Replace ["a", "b", "c"])]

      it "apply (delete 3 $ insert 0 \"a\" $ insert 0 \"b\" $ insert 0 \"c\" empty) [\"0\", \"1\", \"2\", \"3\"] = [\"a\", \"b\", \"c\", \"1\", \"2\", \"3\"]" $ do
        SequenceDiff.apply
          (SequenceDiff.delete 3 $
           SequenceDiff.insert 0 "a" $
           SequenceDiff.insert 0 "b" $
           SequenceDiff.insert 0 "c" $
           SequenceDiff.empty
          ) ["0", "1", "2", "3"] `shouldBe`
          ["a", "b", "c", "1", "2", "3"]

      it "toList (replace 0 \"b\" empty) = [(0, Replace [\"b\"])]" $ do
        SequenceDiff.toList (SequenceDiff.replace 0 "b" $ SequenceDiff.empty) `shouldBe`
          [(0, SequenceDiff.Replace ["b"])]

      it "apply (replace 0 \"b\" empty) [\"a\"] = [\"b\"]" $ do
        SequenceDiff.apply (SequenceDiff.replace 0 "b" $ SequenceDiff.empty) ["a"] `shouldBe` ["b"]

      it "toList (replace 2 \"b\" $ insert 0 \"a\" empty)" $ do
        SequenceDiff.toList (SequenceDiff.replace 2 "b" $ SequenceDiff.insert 0 "a" SequenceDiff.empty) `shouldBe`
          [(0, SequenceDiff.Insert ["a"]), (1, SequenceDiff.Replace ["b"])]

      it "apply (replace 2 \"b\" $ insert 0 \"a\" empty) [\"0\", \"1\", \"2\"] = [\"a\", \"0\", \"b\", \"2\"]" $ do
        SequenceDiff.apply (SequenceDiff.replace 2 "b" $ SequenceDiff.insert 0 "a" SequenceDiff.empty) ["0", "1", "2"] `shouldBe`
          ["a", "0", "b", "2"]

      it "toList (replace 2 \"c\" $ insert 0 \"b\" $ insert 0 \"a\" empty) = [(0, Replace [\"b\", \"a\", \"c\"])]" $ do
        SequenceDiff.toList (SequenceDiff.replace 2 "c" $ SequenceDiff.insert 0 "b" $ SequenceDiff.insert 0 "a" SequenceDiff.empty) `shouldBe`
          [(0, SequenceDiff.Replace ["b", "a", "c"])]

      it "apply (replace 2 \"c\" $ insert 0 \"b\" $ insert 0 \"a\" empty) [\"0\", \"1\", \"2\"] = [\"b\", \"a\", \"c\", \"1\", \"2\"]" $ do
        SequenceDiff.apply (SequenceDiff.replace 2 "c" $ SequenceDiff.insert 0 "b" $ SequenceDiff.insert 0 "a" SequenceDiff.empty) ["0", "1", "2"] `shouldBe`
          ["b", "a", "c", "1", "2"]

      it "apply (replace 0 \"a\" empty) [\"x\"] = [\"a\"]" $ do
        SequenceDiff.apply (SequenceDiff.replace 0 "a" SequenceDiff.empty) ["x"] `shouldBe`
          ["a"]

      it "toList (insert 0 \"y\" $ replace 0 \"x\" empty) = [(0, Replace [\"y\", \"x\"])]" $ do
        SequenceDiff.toList (SequenceDiff.insert 0 "y" $ SequenceDiff.replace 0 "x" SequenceDiff.empty) `shouldBe`
          [(0, SequenceDiff.Replace ["y", "x"])]

      it "apply (insert 0 \"y\" $ replace 0 \"x\" empty) [\"0\", \"1\"] = [\"y\", \"x\", \"1\"]" $ do
        SequenceDiff.apply (SequenceDiff.insert 0 "y" $ SequenceDiff.replace 0 "x" SequenceDiff.empty) ["0", "1"] `shouldBe`
          ["y", "x", "1"]

      it "toList (insert 3 \"a\" $ replace 1 \"b\" $ insert 0 \"c\" empty) = [(0, Replace [\"a\", \"b\"]), (2, Insert [\"c\"])]" $ do
        SequenceDiff.toList
          (SequenceDiff.insert 3 "a" $
           SequenceDiff.replace 1 "b" $
           SequenceDiff.insert 0 "c" $
           SequenceDiff.empty
          ) `shouldBe`
          [(0, SequenceDiff.Replace ["c", "b"]), (2, SequenceDiff.Insert ["a"])]

      it "trace 1" $ do
        let
          -- ["0", "1"]
          _0 = SequenceDiff.empty
          -- ["0", "1"]
          _1 = SequenceDiff.insert 0 "a" _0
          -- ["a", "0", "1"]
          _2 = SequenceDiff.replace 1 "b" _1
          -- ["a", "b", "1"]
          _3 = SequenceDiff.insert 3 "c" _2
          -- ["a", "b", "1", "c"]
          _4 = SequenceDiff.insert 0 "d" _3
          -- ["d", "a", "b", "1", "c"]
          final = _4

        SequenceDiff.toList _2 `shouldBe` [(0, SequenceDiff.Replace ["a", "b"])]
        SequenceDiff.apply _2 ["0", "1"] `shouldBe` ["a", "b", "1"]

        SequenceDiff.toList _3 `shouldBe` [(0, SequenceDiff.Replace ["a", "b"]), (2, SequenceDiff.Insert ["c"])]
        SequenceDiff.apply _3 ["0", "1"] `shouldBe` ["a", "b", "1", "c"]

        SequenceDiff.toList _4 `shouldBe` [(0, SequenceDiff.Replace ["d", "a", "b"]), (2, SequenceDiff.Insert ["c"])]
        SequenceDiff.apply _4 ["0", "1"] `shouldBe` ["d", "a", "b", "1", "c"]

        SequenceDiff.toList final `shouldBe`
          [(0, SequenceDiff.Replace ["d", "a", "b"]), (2, SequenceDiff.Insert ["c"])]

        SequenceDiff.apply final ["0", "1"] `shouldBe`
          ["d", "a", "b", "1", "c"]

      it "trace 2" $ do
        let
          -- ["0", "1"]
          _0 = SequenceDiff.empty
          -- ["0", "1"]
          _1 = SequenceDiff.delete 0 _0
          -- ["1"]
          _2 = SequenceDiff.insert 1 "a" _1
          -- ["1", "a"]

        SequenceDiff.toList _2 `shouldBe` [(0, SequenceDiff.Delete), (2, SequenceDiff.Insert ["a"])]
        SequenceDiff.apply _2 ["0", "1"] `shouldBe` ["1", "a"]

      it "trace 3" $ do
        let
          -- ["0", "1", "2"]
          _0 = SequenceDiff.empty
          -- ["0", "1", "2"]
          _1 = SequenceDiff.delete 1 _0
          -- ["0", "2"]
          _2 = SequenceDiff.replace 1 "a" _1
          -- ["0", "a"]

        SequenceDiff.toList _1 `shouldBe` [(1, SequenceDiff.Delete)]
        SequenceDiff.apply _1 ["0", "1", "2"] `shouldBe` ["0", "2"]

        SequenceDiff.toList _2 `shouldBe` [(1, SequenceDiff.Delete), (2, SequenceDiff.Replace ["a"])]
        SequenceDiff.apply _2 ["0", "1", "2"] `shouldBe` ["0", "a"]

    describe "properties" $ do
      let numTests = 100
      modifyMaxSuccess (const numTests) . it "forall ix val cs xs. insert ix val (apply cs xs) = apply (insert ix val cs) xs" . hedgehog $ do
        xs <- forAll $ Gen.list (Range.constant 0 100) genVal
        ix <- forAll $ Gen.int (Range.constant 0 $ length xs) -- you can insert at the end of the list
        val <- forAll genVal
        cs <- forAll $ genSequenceDiff genVal
        insert ix val (SequenceDiff.apply cs xs) ===
          SequenceDiff.apply (SequenceDiff.insert ix val cs) xs

      modifyMaxSuccess (const numTests) . it "forall ix val cs xs. length xs > 0 ==> replace ix val (apply cs xs) = apply (replace ix val cs) xs" . hedgehog $ do
        xs <- forAll $ Gen.list (Range.constant 1 100) genVal
        ix <- forAll $ Gen.int (Range.constant 0 $ length xs - 1) -- you have to replace an element
        val <- forAll genVal
        cs <- forAll $ genSequenceDiff genVal
        replace ix val (SequenceDiff.apply cs xs) ===
          SequenceDiff.apply (SequenceDiff.replace ix val cs) xs

      modifyMaxSuccess (const numTests) . it "forall ix val cs. delete ix (insert ix val cs) = cs" . hedgehog $ do
        ix <- forAll $ Gen.int (Range.constant 0 maxBound)
        val <- forAll genVal
        cs <- forAll $ genSequenceDiff genVal
        SequenceDiff.delete ix (SequenceDiff.insert ix val cs) ===
          cs

      modifyMaxSuccess (const numTests) . it "forall ix val xs. length xs > 0 ==> apply (delete (ix + 1) $ insert ix val empty) xs = replace ix val xs" . hedgehog $ do
        xs <- forAll $ Gen.list (Range.constant 1 100) genVal -- nonempty
        ix <- forAll $ Gen.int (Range.constant 0 $ length xs - 1)
        val <- forAll genVal
        SequenceDiff.apply (SequenceDiff.delete (ix+1) $ SequenceDiff.insert ix val SequenceDiff.empty) xs ===
          replace ix val xs

      modifyMaxSuccess (const numTests) . it "forall ix cs xs. length xs > 0 ==> delete ix (apply cs xs) = apply (delete ix cs) xs" . hedgehog $ do
        xs <- forAll $ Gen.list (Range.constant 1 100) genVal
        ix <- forAll $ Gen.int (Range.constant 0 $ length xs - 1) -- you can only delete elements of the list
        cs <- forAll $ genSequenceDiff genVal
        delete ix (SequenceDiff.apply cs xs) ===
          SequenceDiff.apply (SequenceDiff.delete ix cs) xs
