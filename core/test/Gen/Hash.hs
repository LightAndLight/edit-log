{-# language GADTs #-}
module Gen.Hash
  ( genHashOf
  , genHash
  , genEqualHash
  , genEqualHashes
  )
where

import Data.Some (Some(..))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Gen.NodeType (SomeNodeType(..), genNodeType)

import Hash (Hash(..))
import NodeType (NodeType(..))

genInt :: Gen Int
genInt = Gen.int (Range.constant minBound maxBound)

genHashOf :: NodeType a -> Gen (Hash a)
genHashOf ty = do
  n <- genInt
  pure $ case ty of
    TBlock -> HBlock n
    TExpr -> HExpr n
    TStatement -> HStatement n
    TIdent -> HIdent n
    TArgs -> HArgs n
    TParams -> HParams n

genHash :: Gen (Some Hash)
genHash = do
  SomeNodeType ty <- genNodeType
  Some <$> genHashOf ty

genEqualHash :: Some Hash -> Gen (Some Hash)
genEqualHash h = pure h

genEqualHashes :: Gen (Some Hash, Some Hash)
genEqualHashes = do
  n <- genInt
  Gen.element
    [ (Some $ HBlock n, Some $ HBlock n)
    , (Some $ HExpr n, Some $ HExpr n)
    , (Some $ HStatement n, Some $ HStatement n)
    , (Some $ HIdent n, Some $ HIdent n)
    , (Some $ HArgs n, Some $ HArgs n)
    , (Some $ HParams n, Some $ HParams n)
    ]
