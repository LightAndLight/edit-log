{-# language GeneralisedNewtypeDeriving #-}
{-# language DeriveGeneric #-}
module Syntax where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype Ident = Ident String
  deriving (Eq, Hashable, Show)

data Block
  = Block [Statement]
  | BHole

data Statement
  = For Ident Expr Block
  | IfThen Expr Block
  | IfThenElse Expr Block Block
  | SHole

data BinOp
  = Add
  | Sub
  | Mul
  | Div

  | Or
  | And
  deriving (Eq, Generic, Show)
instance Hashable BinOp

data UnOp
  = Neg

  | Not
  deriving (Eq, Generic, Show)
instance Hashable UnOp

data Expr
  = Bool Bool
  | Int Int
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | EHole
  deriving Show
