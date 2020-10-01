{-# language GADTs #-}
module Gen.NodeType
  ( genNodeType
  , genEqualNodeType
  , genEqualNodeTypes
  )
where

import Data.Some (Some(..))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

import NodeType (NodeType(..))

genNodeType :: Gen (Some NodeType)
genNodeType = Gen.element [Some TBlock, Some TExpr, Some TStatement]

genEqualNodeType :: Some NodeType -> Gen (Some NodeType)
genEqualNodeType (Some ty) =
  case ty of
    TBlock -> pure $ Some TBlock
    TExpr -> pure $ Some TExpr
    TStatement -> pure $ Some TStatement

genEqualNodeTypes :: Gen (Some NodeType, Some NodeType)
genEqualNodeTypes =
  Gen.element
  [ (Some TBlock, Some TBlock)
  , (Some TExpr, Some TExpr)
  , (Some TStatement, Some TStatement)
  ]
