module Main where

import Test.Hspec

import Test.Hash (hashSpec)
import Test.NodeType (nodeTypeSpec)

import qualified Store

main :: IO ()
main =
  hspec $ do
    hashSpec
    nodeTypeSpec
    describe "Store.insertAll" $ do
      it "insertAll [(0, [x, y])] [a, b] = [x, y, a, b]" $ do
        Store.insertAll '_' [(0, "xy")] "ab" `shouldBe` "xyab"

      it "insertAll [(0, [x, y]), (1, [z, w])] [a, b] = [x, y, a, z, w, b]" $ do
        Store.insertAll '_' [(0, "xy"), (1, "zw")] "ab" `shouldBe` "xyazwb"

      it "insertAll [(1, [z, w]), (0, [x, y])] [a, b] = [x, y, a, z, w, b]" $ do
        Store.insertAll '_' [(1, "zw"), (0, "xy")] "ab" `shouldBe` "xyazwb"

      it "insertAll [(2, [z, w])] [a, b] = [a, b, z, w]" $ do
        Store.insertAll '_' [(2, "zw")] "ab" `shouldBe` "abzw"

      it "insertAll [(3, [z, w])] [a, b] = [a, b, _, z, w]" $ do
        Store.insertAll '_' [(3, "zw")] "ab" `shouldBe` "ab_zw"

      it "insertAll [(4, [z, w])] [a, b] = [a, b, _, _, z, w]" $ do
        Store.insertAll '_' [(4, "zw")] "ab" `shouldBe` "ab__zw"

      it "insertAll [(2, [z, w])] [] = [_, _, z, w]" $ do
        Store.insertAll '_' [(2, "zw")] "" `shouldBe` "__zw"
