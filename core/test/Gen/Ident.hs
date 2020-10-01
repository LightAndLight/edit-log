module Gen.Ident (genIdent) where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Syntax (Ident(..))

genIdent :: Gen Ident
genIdent = Ident <$> Gen.string (Range.constant 5 15) Gen.alpha
