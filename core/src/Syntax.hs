{-# language GeneralisedNewtypeDeriving #-}
{-# language DeriveGeneric #-}
module Syntax where

import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

data Ident
  = Ident String
  | IHole
  deriving (Eq, Ord, Generic, Show)
instance Hashable Ident

newtype Block
  = Block (NonEmpty Statement)
  deriving Show

newtype List a = List [a]
  deriving (Eq, Show, Hashable)

data Statement
  = For Ident Expr Block
  | IfThen Expr Block
  | IfThenElse Expr Block Block
  | Print Expr
  | Return Expr
  | Def Ident (List Ident) Block
  | SHole
  deriving Show

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

data Expr
  = Bool Bool
  | Int Int
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | Call Expr (List Expr)
  | EIdent String
  | EHole
  deriving Show
