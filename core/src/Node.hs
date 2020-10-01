{-# language GADTs, KindSignatures, ScopedTypeVariables, TypeApplications #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module Node where

import Data.Hashable (Hashable(..))
import Data.GADT.Compare (geq)
import Data.GADT.Compare.TH (deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Type.Equality ((:~:))

import Hash (Hash(..))
import NodeType (KnownNodeType)
import Syntax (Expr, Statement, Block, UnOp, BinOp, Ident)

data Node :: * -> * where
  NFor :: Ident -> Hash Expr -> Hash Block -> Node Statement
  NIfThen :: Hash Expr -> Hash Block -> Node Statement
  NIfThenElse :: Hash Expr -> Hash Block -> Hash Block -> Node Statement

  NBool :: Bool -> Node Expr
  NInt :: Int -> Node Expr
  NBinOp :: BinOp -> Hash Expr -> Hash Expr -> Node Expr
  NUnOp :: UnOp -> Hash Expr -> Node Expr

  NBlock :: [Hash Statement] -> Node Block

  NSHole :: Node Statement
  NEHole :: Node Expr
deriving instance Show (Node a)
deriveGEq ''Node
deriveGShow ''Node

eqNode :: Node a -> Node b -> Maybe (a :~: b)
eqNode = geq

instance Eq (Node a) where
  n1 == n2 =
    case eqNode n1 n2 of
      Nothing -> False
      Just{} -> True

instance Hashable (Node a) where
  hashWithSalt s n =
    case n of
      NFor ident ex body -> hashWithSalt s (0::Int, ident, ex, body)
      NIfThen cond then_ -> hashWithSalt s (1::Int, cond, then_)
      NIfThenElse cond then_ else_ -> hashWithSalt s (2::Int, cond, then_, else_)

      NBool b -> hashWithSalt s (3::Int, b)
      NInt v -> hashWithSalt s (4::Int, v)
      NBinOp op l r -> hashWithSalt s (5::Int, op, l, r)
      NUnOp op ex -> hashWithSalt s (6::Int, op, ex)

      NBlock sts -> hashWithSalt s (7::Int, sts)

      NSHole -> hashWithSalt s (8::Int)
      NEHole -> hashWithSalt s (9::Int)

hashNode :: forall a. KnownNodeType a => Node a -> Hash a
hashNode n =
  case n of
    NFor{} -> HStatement $ hash n
    NIfThen{} -> HStatement $ hash n
    NIfThenElse{} -> HStatement $ hash n

    NBool{} -> HExpr $ hash n
    NInt{} -> HExpr $ hash n
    NBinOp{} -> HExpr $ hash n
    NUnOp{} -> HExpr $ hash n

    NBlock{} -> HBlock $ hash n

    NSHole -> HStatement $ hash n
    NEHole -> HExpr $ hash n
