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

import Control.Monad (guard)
import Data.Hashable (Hashable(..))
import Data.GADT.Show.TH (deriveGShow)
import Data.Type.Equality ((:~:)(Refl))

import NodeType (NodeType(..))
import Syntax (Block, Expr, Statement)

data Hash :: * -> * where
  HExpr :: Int -> Hash Expr
  HStatement :: Int -> Hash Statement
  HBlock :: Int -> Hash Block
deriveGShow ''Hash
deriving instance Show (Hash a)

instance Hashable (Hash a) where; hashWithSalt s = hashWithSalt s . unHash
instance Eq (Hash a) where; h1 == h2 = unHash h1 == unHash h2

eqHash :: Hash a -> Hash b -> Maybe (a :~: b)
eqHash h1 h2 =
  case h1 of
    HExpr n ->
      case h2 of
        HExpr n' -> do
          guard (n == n')
          pure Refl
        _ -> Nothing
    HStatement n ->
      case h2 of
        HStatement n' -> do
          guard (n == n')
          pure Refl
        _ -> Nothing
    HBlock n ->
      case h2 of
        HBlock n' -> do
          guard (n == n')
          pure Refl
        _ -> Nothing

mkHash :: NodeType a -> Int -> Hash a
mkHash nt h =
  case nt of
    TExpr -> HExpr h
    TStatement -> HStatement h
    TBlock -> HBlock h

unHash :: Hash a -> Int
unHash h =
  case h of
    HExpr n -> n
    HStatement n -> n
    HBlock n -> n
