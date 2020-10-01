{-# language AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}
{-# language BangPatterns #-}
{-# language GADTs #-}
module Test.Store.Pure (storePureSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.List as List
import Data.Some (Some(..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Gen.Node (SomeNode(..), genNode)

import Hash (Hash)
import Node (Node(..))
import qualified Store
import Store.Pure (runStoreT, newStore)
import Store.Pure.Internal (Store(..), Entry(..))

storeValid :: Store -> Bool
storeValid (Store htn nth) =
  and
  [ List.nub hashDomain == hashDomain
  , List.nub nodeDomain == nodeDomain

  , all (`elem` hashCodomain) hashDomain && all (`elem` hashDomain) hashCodomain
  , all (`elem` nodeCodomain) nodeDomain && all (`elem` nodeDomain) nodeCodomain
  ]
  where
    hashDomain :: [Some Hash]
    hashDomain = (\(Entry h _) -> Some h) <$> htn

    hashCodomain :: [Some Hash]
    hashCodomain = (\(Entry _ h) -> Some h) <$> nth

    nodeDomain :: [Some Node]
    nodeDomain = (\(Entry n _) -> Some n) <$> nth

    nodeCodomain :: [Some Node]
    nodeCodomain = (\(Entry _ n) -> Some n) <$> htn

genStore :: Gen Store
genStore = Gen.list (Range.constant 1 100) genNode >>= go newStore
  where
    go !store nodes =
      case nodes of
        [] -> pure store
        SomeNode node : rest ->
          let
            Identity (_, store') = runStoreT store $ Store.addNode node
          in
            go store' rest

genHashInStore :: Store -> Gen (Some Hash)
genHashInStore (Store htn nth) =
  Gen.element $
  fmap (\(Entry h _) -> Some h) htn <>
  fmap (\(Entry _ h) -> Some h) nth

genNodeInStore :: Store -> Gen (Some Node)
genNodeInStore (Store htn nth) =
  Gen.element $
  fmap (\(Entry _ n) -> Some n) htn <>
  fmap (\(Entry n _) -> Some n) nth

storePureSpec :: Spec
storePureSpec =
  describe "store pure" $ do
    it "newStore valid" $ do
      newStore `shouldSatisfy` storeValid
    it "addNodes valid" . hedgehog $ do
      nodes <- forAll $ Gen.list (Range.constant 1 100) genNode
      storeRef <- liftIO $ newIORef newStore
      for_ nodes $ \(SomeNode node) -> do
        input <- liftIO $ readIORef storeRef
        let Identity (_, output) = runStoreT input $ Store.addNode node
        assert $ storeValid output
        liftIO $ writeIORef storeRef output
    it "lookupNode after lookupHash" . hedgehog $ do
      store <- forAll genStore
      Some h <- forAll $ genHashInStore store
      let
        Identity (h', _) = runStoreT store $ do
          m_n <- Store.lookupNode h
          case m_n of
            Nothing -> error $ "store is missing " <> show h
            Just n -> do
              m_h' <- Store.lookupHash n
              maybe (error $ "store is missing " <> show n) pure m_h'
      h === h'
    it "lookupHash after lookupNode" . hedgehog $ do
      store <- forAll genStore
      Some n <- forAll $ genNodeInStore store
      let
        Identity (n', _) = runStoreT store $ do
          m_h <- Store.lookupHash n
          case m_h of
            Nothing -> error $ "store is missing " <> show n
            Just h -> do
              m_n' <- Store.lookupNode h
              maybe (error $ "store is missing " <> show h) pure m_n'
      n === n'
{-
  describe ("store pure" <> ty) $ do
    describe "generators" $ do
      _

-}
