module Test.NodeType (nodeTypeSpec) where

import Data.Maybe (isJust)
import Data.Some (Some(..))
import Hedgehog
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Gen.NodeType (SomeNodeType(..), genNodeType, genEqualNodeType, genEqualNodeTypes)

import NodeType (NodeType, eqNodeType)

equalNodeTypes :: NodeType a -> NodeType b -> PropertyT IO ()
equalNodeTypes t1 t2 = diff t1 (\a b -> isJust $ eqNodeType a b) t2

nodeTypeSpec :: Spec
nodeTypeSpec = do
  describe "nodeType" $ do
    describe "generators" $ do
      it "genEqualNodeType" . hedgehog $ do
        SomeNodeType ty <- forAll genNodeType
        Some ty' <- forAll $ genEqualNodeType (Some ty)
        equalNodeTypes ty ty'
      it "genEqualNodeTypes" . hedgehog $ do
        (Some ty, Some ty') <- forAll genEqualNodeTypes
        equalNodeTypes ty ty'
    describe "eqNodeType" $ do
      it "reflexive" . hedgehog $ do
        SomeNodeType t <- forAll genNodeType
        equalNodeTypes t t
      it "transitive" . hedgehog $ do
        SomeNodeType t1 <- forAll genNodeType
        Some t2 <- forAll $ genEqualNodeType (Some t1)
        Some t3 <- forAll $ genEqualNodeType (Some t2)
        equalNodeTypes t1 t3
      it "symmetric" . hedgehog $ do
        (Some t1, Some t2) <- forAll genEqualNodeTypes
        equalNodeTypes t2 t1
