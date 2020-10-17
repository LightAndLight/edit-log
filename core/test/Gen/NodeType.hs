{-# language GADTs #-}
{-# language StandaloneDeriving #-}
module Gen.NodeType
  ( SomeNodeType(..)
  , genNodeType
  , genEqualNodeType
  , genEqualNodeTypes
  )
where

import Data.Some (Some(..))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

import NodeType (KnownNodeType, NodeType(..))

data SomeNodeType where
  SomeNodeType :: KnownNodeType a => NodeType a -> SomeNodeType
deriving instance Show SomeNodeType

genNodeType :: Gen SomeNodeType
genNodeType = Gen.element [SomeNodeType TBlock, SomeNodeType TExpr, SomeNodeType TStatement]

genEqualNodeType :: Some NodeType -> Gen (Some NodeType)
genEqualNodeType (Some ty) =
  case ty of
    TBlock -> pure $ Some TBlock
    TExpr -> pure $ Some TExpr
    TStatement -> pure $ Some TStatement
    TIdent -> pure $ Some TIdent
    TArgs -> pure $ Some TArgs
    TParams -> pure $ Some TParams

genEqualNodeTypes :: Gen (Some NodeType, Some NodeType)
genEqualNodeTypes =
  Gen.element
  [ (Some TBlock, Some TBlock)
  , (Some TExpr, Some TExpr)
  , (Some TStatement, Some TStatement)
  , (Some TIdent, Some TIdent)
  , (Some TArgs, Some TArgs)
  , (Some TParams, Some TParams)
  ]
