{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language GeneralisedNewtypeDeriving #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# options_ghc -fno-warn-overlapping-patterns #-}
module Syntax where

import Control.Lens.TH (makePrisms)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import GHC.Generics (Generic)

import Sequence (IsSequence(..))

data Ident
  = Ident String
  | IHole
  deriving (Eq, Ord, Generic, Show)
instance Hashable Ident

newtype Params = Params [Ident]
  deriving (Eq, Show)
makePrisms ''Params

instance IsSequence Params where
  type Item Params = Ident
  deleteAt ix (Params xs) = Params $ deleteAt ix xs
  insertAt ix val (Params xs) = Params $ insertAt ix val xs
  insertAll def vals (Params xs) = Params $ insertAll def vals xs

data Statement
  = For Ident Expr Block
  | IfThen Expr Block
  | IfThenElse Expr Block Block
  | Print Expr
  | Return Expr
  | Def Ident Params Block
  | SHole
  deriving (Eq, Show)

newtype Block
  = Block (NonEmpty Statement)
  deriving (Eq, Show)

instance IsSequence Block where
  type Item Block = Statement
  deleteAt ix (Block sts) =
    Block . Maybe.fromMaybe (pure SHole) . NonEmpty.nonEmpty $
    deleteAt ix (NonEmpty.toList sts)

  insertAt ix val (Block sts) =
    Block . Maybe.fromMaybe (pure SHole) . NonEmpty.nonEmpty $
    insertAt ix val (NonEmpty.toList sts)

  insertAll def vals (Block sts) =
    Block . Maybe.fromMaybe (pure SHole) . NonEmpty.nonEmpty $
    insertAll def vals (NonEmpty.toList sts)

data BinOp
  = Add
  | Sub
  | Mul
  | Div

  | Eq

  | Or
  | And
  deriving (Eq, Generic, Show, Enum, Bounded)
instance Hashable BinOp

data UnOp
  = Neg

  | Not
  deriving (Eq, Generic, Show, Enum, Bounded)
instance Hashable UnOp

newtype Args = Args [Expr]
  deriving (Eq, Show)

instance IsSequence Args where
  type Item Args = Expr
  deleteAt ix (Args xs) = Args $ deleteAt ix xs
  insertAt ix val (Args xs) = Args $ insertAt ix val xs
  insertAll def vals (Args xs) = Args $ insertAll def vals xs

newtype Exprs = Exprs [Expr]
  deriving (Eq, Show)

instance IsSequence Exprs where
  type Item Exprs = Expr
  deleteAt ix (Exprs xs) = Exprs $ deleteAt ix xs
  insertAt ix val (Exprs xs) = Exprs $ insertAt ix val xs
  insertAll def vals (Exprs xs) = Exprs $ insertAll def vals xs

data Expr
  = Bool Bool
  | Int Int
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | Call Expr Args
  | List Exprs
  | EIdent String
  | EHole
  deriving (Eq, Show)

makePrisms ''Args
