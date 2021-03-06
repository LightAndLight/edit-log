{-# language GADTs, KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module Hash
  ( Hash(..)
  , eqHash
  , mkHash
  , unHash
  )
where

import Data.Hashable (Hashable(..))
import Data.GADT.Compare (geq)
import Data.GADT.Compare.TH (deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Type.Equality ((:~:))

import NodeType (NodeType(..))
import Syntax (Block, Expr, Ident, Statement, Args, Params, Exprs)

data Hash :: * -> * where
  HExpr :: Int -> Hash Expr
  HStatement :: Int -> Hash Statement
  HBlock :: Int -> Hash Block
  HIdent :: Int -> Hash Ident
  HArgs :: Int -> Hash Args
  HParams :: Int -> Hash Params
  HExprs :: Int -> Hash Exprs
deriveGShow ''Hash
deriveGEq ''Hash
deriving instance Show (Hash a)

instance Hashable (Hash a) where; hashWithSalt s = hashWithSalt s . unHash
instance Eq (Hash a) where; h1 == h2 = unHash h1 == unHash h2

eqHash :: Hash a -> Hash b -> Maybe (a :~: b)
eqHash = geq

mkHash :: NodeType a -> Int -> Hash a
mkHash nt h =
  case nt of
    TExpr -> HExpr h
    TStatement -> HStatement h
    TBlock -> HBlock h
    TIdent -> HIdent h
    TArgs -> HArgs h
    TParams -> HParams h
    TExprs -> HExprs h

unHash :: Hash a -> Int
unHash h =
  case h of
    HExpr n -> n
    HStatement n -> n
    HBlock n -> n
    HIdent n -> n
    HArgs n -> n
    HParams n -> n
    HExprs n -> n
