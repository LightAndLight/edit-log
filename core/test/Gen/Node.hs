{-# language GADTs #-}
{-# language StandaloneDeriving #-}
module Gen.Node
  ( genNodeOf
  , SomeNode(..)
  , genNode
  )
where

import Hedgehog (Gen)
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

import Gen.Hash (genHashOf)
import Gen.Ident (genIdent)
import Gen.NodeType (SomeNodeType(..), genNodeType)
import Gen.Syntax (genBinOp, genUnOp)

import Node (Node(..))
import NodeType (KnownNodeType, NodeType(..))
import Syntax (Ident(..))

genNodeOf :: NodeType a -> Gen (Node a)
genNodeOf nt =
  case nt of
    TBlock ->
      NBlock <$> Gen.nonEmpty (Range.linear 1 5) (genHashOf TStatement)
    TExpr ->
      Gen.choice
      [ NBool <$> Gen.bool
      , NInt <$> Gen.int (Range.constant minBound maxBound)
      , NBinOp <$> genBinOp <*> genHashOf TExpr <*> genHashOf TExpr
      , NUnOp <$> genUnOp <*> genHashOf TExpr
      , pure NEHole
      ]
    TStatement ->
      Gen.choice
      [ NFor <$> genHashOf TIdent <*> genHashOf TExpr <*> genHashOf TBlock
      , NIfThen <$> genHashOf TExpr <*> genHashOf TBlock
      , NIfThenElse <$> genHashOf TExpr <*> genHashOf TBlock <*> genHashOf TBlock
      , pure NSHole
      ]
    TIdent -> do
      Ident i <- genIdent
      pure $ NIdent i
    TArgs -> NArgs <$> Gen.list (Range.constant 0 10) (genHashOf TExpr)
    TParams -> NParams <$> Gen.list (Range.constant 0 10) (genHashOf TIdent)
    TExprs -> NExprs <$> Gen.list (Range.constant 0 10) (genHashOf TExpr)

data SomeNode where
  SomeNode :: KnownNodeType a => Node a -> SomeNode
deriving instance Show SomeNode

genNode :: Gen SomeNode
genNode = do
  SomeNodeType ty <- genNodeType
  SomeNode <$> genNodeOf ty
