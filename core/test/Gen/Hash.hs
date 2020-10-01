{-# language GADTs #-}
module Gen.Hash
  ( genHash
  , genEqualHash
  , genEqualHashes
  )
where

import Data.Some (Some(..))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hash (Hash(..))

genInt :: Gen Int
genInt = Gen.int (Range.constant minBound maxBound)

genHash :: Gen (Some Hash)
genHash = do
  n <- genInt
  Gen.element
    [ Some $ HBlock n
    , Some $ HExpr n
    , Some $ HStatement n
    ]

genEqualHash :: Some Hash -> Gen (Some Hash)
genEqualHash h = pure h

genEqualHashes :: Gen (Some Hash, Some Hash)
genEqualHashes = do
  n <- genInt
  Gen.element
    [ (Some $ HBlock n, Some $ HBlock n)
    , (Some $ HExpr n, Some $ HExpr n)
    , (Some $ HStatement n, Some $ HStatement n)
    ]
