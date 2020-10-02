module Test.Diff.DeleteSet (deleteSetSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Foldable (for_)
import Data.List (nub, sort)
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

import Diff.DeleteSet (DeleteSet)
import qualified Diff.DeleteSet as DeleteSet

valid :: DeleteSet -> Bool
valid ds =
  distinct
  where
    dsList = DeleteSet.toList ds

    distinct = nub dsList == dsList

deleteSetSpec :: Spec
deleteSetSpec =
  describe "DeleteSet" $ do
    it "sort . toList $ insert 0 (insert 0 empty) = [0, 1]" $ do
      sort (DeleteSet.toList $ DeleteSet.insert 0 (DeleteSet.insert 0 DeleteSet.empty)) `shouldBe` [0, 1]
    it "sort . toList $ insert 0 (insert 0 (insert 0 empty)) = [0, 1, 2]" $ do
      sort (DeleteSet.toList $ DeleteSet.insert 0 (DeleteSet.insert 0 (DeleteSet.insert 0 DeleteSet.empty))) `shouldBe` [0, 1, 2]
    it "valid empty" $ do
      DeleteSet.empty `shouldSatisfy` valid
    it "forall ix ds. valid ds ==> valid (insert ix ds)" . hedgehog $ do
      vals <- forAll $ Gen.list (Range.constant 1 100) (Gen.int $ Range.constant 0 1000)
      ref <- liftIO $ newIORef DeleteSet.empty
      for_ vals $ \val -> do
        input <- liftIO $ readIORef ref
        let output = DeleteSet.insert val input
        assert $ valid output
        liftIO $ writeIORef ref output
