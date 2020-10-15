{-# language TemplateHaskell #-}
module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Autocomplete (similarity)

numTests :: TestLimit
numTests = 100

prop_similarity_sane :: Property
prop_similarity_sane =
  withTests numTests . property $ do
    needle <- forAll $ Gen.text (Range.constant 1 10) Gen.ascii
    haystack <-
      forAll $
      (\a b -> a <> needle <> b) <$>
      Gen.text (Range.constant 0 10) Gen.ascii <*>
      Gen.text (Range.constant 0 10) Gen.ascii
    let score = similarity needle haystack
    score /== 0
    assert $ score <= 1.0
    if needle == haystack
      then score === 1.0
      else success

main :: IO Bool
main =
  checkParallel $$(discover)
  
