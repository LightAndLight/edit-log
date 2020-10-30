{-# language GADTs #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language StandaloneDeriving #-}
module Gen.Path (SomePath(..), genPathFor, genPath, KnownLevel(..), genLevelFor) where

import Control.Applicative (empty)
import Data.Some (Some(..))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import NodeType (KnownNodeType, NodeType(..), nodeType)
import Path (Path(..), Level(..))

import Gen.NodeType (SomeNodeType(..), genNodeType)

data KnownLevel a where
  KnownLevel :: KnownNodeType b => Level a b -> KnownLevel a

genLevelFor :: NodeType a -> Gen (KnownLevel a)
genLevelFor nt =
  case nt of
    TExpr ->
      Gen.element
      [ KnownLevel BinOp_Left
      , KnownLevel BinOp_Right
      , KnownLevel UnOp_Value
      , KnownLevel Call_Function
      , KnownLevel Call_Args
      ]
    TStatement ->
      Gen.element
      [ KnownLevel For_Ident
      , KnownLevel For_Expr
      , KnownLevel For_Block
      , KnownLevel IfThen_Cond
      , KnownLevel IfThen_Then
      , KnownLevel IfThenElse_Cond
      , KnownLevel IfThenElse_Then
      , KnownLevel IfThenElse_Else
      , KnownLevel Print_Value
      , KnownLevel Return_Value
      , KnownLevel Def_Name
      , KnownLevel Def_Args
      , KnownLevel Def_Body
      ]
    TBlock -> KnownLevel . Block_Index <$> Gen.int (Range.constant 0 maxBound)
    TArgs -> KnownLevel . Args_Index <$> Gen.int (Range.constant 0 maxBound)
    TParams -> KnownLevel . Params_Index <$> Gen.int (Range.constant 0 maxBound)
    TExprs -> KnownLevel . Exprs_Index <$> Gen.int (Range.constant 0 maxBound)
    TIdent -> empty

genPathFor :: forall a. NodeType a -> Gen (Some (Path a))
genPathFor nt =
  Gen.recursive Gen.choice
  [ pure $ Some Nil ]
  [ do
      KnownLevel (level :: Level a b) <- genLevelFor nt
      Some rest <- genPathFor (nodeType @b)
      pure . Some $ Cons level rest
  ]

data SomePath where
  SomePath :: KnownNodeType a => Path a b -> SomePath
deriving instance Show SomePath

genPath :: Gen SomePath
genPath = do
  SomeNodeType nt <-
    let
      loop = do
        nt <- genNodeType
        case nt of
          SomeNodeType TIdent{} -> loop
          _ -> pure nt
     in
       loop
  Some path <- genPathFor nt
  pure $ SomePath path
