{-# language GeneralisedNewtypeDeriving #-}
{-# language DeriveGeneric #-}
module Syntax where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype Ident = Ident String
  deriving (Eq, Hashable, Show)

newtype Block
  = Block [Statement]
  deriving Show

data Statement
  = For Ident Expr Block
  | IfThen Expr Block
  | IfThenElse Expr Block Block
  | Print Expr
  | SHole
  deriving Show

data BinOp
  = Add
  | Sub
  | Mul
  | Div

  | Or
  | And
  deriving (Eq, Generic, Show, Enum, Bounded)
instance Hashable BinOp

data UnOp
  = Neg

  | Not
  deriving (Eq, Generic, Show, Enum, Bounded)
instance Hashable UnOp

data Expr
  = Bool Bool
  | Int Int
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | EHole
  deriving Show
