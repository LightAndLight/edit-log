{-# language GADTs #-}
{-# language QuantifiedConstraints, StandaloneDeriving, UndecidableInstances #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables, TypeApplications #-}
module Gen.Path.Trie (SomeTrie(..), genTrieFor, genTrie) where

import Data.Bifunctor.Flip (Flip(..))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum(..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import NodeType (NodeType(..), nodeType)
import qualified NodeType
import Path (Level)
import Path.Trie.Internal (Trie(..))

import Gen.NodeType (SomeNodeType(..), genNodeType)
import Gen.Path (KnownLevel(..), genLevelFor)

genTrieFor :: forall a f. NodeType a -> (forall x. NodeType x -> Gen (f x)) -> Gen (Trie a f)
genTrieFor nt genLeaf =
  Gen.recursive Gen.choice
  [ Trie <$> Gen.maybe (genLeaf nt) <*> pure mempty ]
  (case nt of
     TIdent ->
       []
     _ ->
       [ do
           mVal <- Gen.maybe $ genLeaf nt
           levels <- fmap DMap.fromList . Gen.list (Range.constant 1 5) $ do
             KnownLevel (level :: Level a b) <- genLevelFor nt
             rest <- genTrieFor (nodeType @b) genLeaf
             pure $ level :=> Flip rest
           pure $ Trie mVal levels
       ]
  )

data SomeTrie f where
  SomeTrie :: Show a => Trie a f -> SomeTrie f
deriving instance (forall x. Show x => Show (f x)) => Show (SomeTrie f)

genTrie :: forall f. (forall x. NodeType x -> Gen (f x)) -> Gen (SomeTrie f)
genTrie genLeaf = do
  SomeNodeType (nt :: NodeType a) <- genNodeType
  NodeType.showingNodeType @a nt $ SomeTrie <$> genTrieFor nt genLeaf
