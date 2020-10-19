{-# language GeneralisedNewtypeDeriving #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# options_ghc -fno-warn-overlapping-patterns #-}
module Syntax where

import Control.Lens.TH (makePrisms)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

data Ident
  = Ident String
  | IHole
  deriving (Eq, Ord, Generic, Show)
instance Hashable Ident

newtype Params = Params [Ident]
  deriving (Eq, Show)
makePrisms ''Params

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

data Expr
  = Bool Bool
  | Int Int
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | Call Expr Args
  | EIdent String
  | EHole
  deriving (Eq, Show)

makePrisms ''Args
