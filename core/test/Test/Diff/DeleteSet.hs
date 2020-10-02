module Test.Diff.DeleteSet (deleteSetSpec) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Foldable (for_)
import Data.List (nub, sort)
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

import qualified Diff.DeleteSet as DeleteSet

valid :: [(Int, a)] -> Bool
valid ds =
  distinct
  where
    distinct = nub (fst <$> ds) == fmap fst ds

insert :: Int -> a -> [(Int, a)] -> [(Int, a)]
insert ix val ds =
  go (ix + length (filter ((<= ix) . fst) ds))
  where
    go i =
      if i `elem` ds
      then go (i+1)
      else (i, val) : ds

deleteSetSpec :: Spec
deleteSetSpec =
  describe "DeleteSet" $ do
    describe "spec" $ do
      it "sort $ insert 0 () (insert 0 () []) = [(0, ()), (1, ())]" $ do
        sort (insert 0 (insert 0 [])) `shouldBe` [0, 1]
      it "sort . toList $ insert 0 () (insert 0 () (insert 0 () [])) = [(0, ()), (1, ()), (2, ())]" $ do
        sort (insert 0 (insert 0 (insert 0 []))) `shouldBe` [0, 1, 2]
      it "forall ix val ds. valid ds ==> valid (insert ix val ds)" . hedgehog $ do
        vals <- forAll $ Gen.list (Range.constant 1 100) (Gen.int $ Range.constant 0 1000)
        ref <- liftIO $ newIORef []
        for_ vals $ \val -> do
          input <- liftIO $ readIORef ref
          let output = insert val () input
          assert $ valid output
          liftIO $ writeIORef ref output
    describe "impl" $ do
      it "empty correct" $ do
        DeleteSet.toList DeleteSet.empty `shouldBe` []
      it "insert correct" . hedgehog $ do
        vals <- forAll $ Gen.list (Range.constant 1 100) (Gen.int $ Range.constant 0 1000)
        ref <- liftIO $ newIORef ([], DeleteSet.empty)
        for_ vals $ \val -> do
          (input, input') <- liftIO $ readIORef ref
          let output = insert val () input
          let output' = DeleteSet.insert val () input'
          sort (fst <$> output) === sort (fst <$> DeleteSet.toList output')
          liftIO $ writeIORef ref (output, output')
