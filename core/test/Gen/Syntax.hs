module Gen.Syntax (genBinOp, genUnOp) where

import Hedgehog (Gen)
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

import Syntax (BinOp(..), UnOp(..))

genBinOp :: Gen BinOp
genBinOp = Gen.element [minBound..maxBound]

genUnOp :: Gen UnOp
genUnOp = Gen.element [minBound..maxBound]
