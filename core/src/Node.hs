{-# language GADTs, KindSignatures, ScopedTypeVariables, TypeApplications #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module Node where

import Data.Hashable (Hashable(..))
import Data.GADT.Compare (GEq, geq)
import Data.GADT.Show.TH (deriveGShow)
import Data.List.NonEmpty (NonEmpty)
import Data.Type.Equality ((:~:)(..))

import Hash (Hash(..))
import NodeType (KnownNodeType, NodeType)
import Syntax (Expr, List, Statement, Block, UnOp, BinOp, Ident)

data Node :: * -> * where
  NFor :: Hash Ident -> Hash Expr -> Hash Block -> Node Statement
  NIfThen :: Hash Expr -> Hash Block -> Node Statement
  NIfThenElse :: Hash Expr -> Hash Block -> Hash Block -> Node Statement
  NPrint :: Hash Expr -> Node Statement
  NReturn :: Hash Expr -> Node Statement
  NDef :: Hash Ident -> Hash (List Ident) -> Hash Block -> Node Statement

  NBool :: Bool -> Node Expr
  NInt :: Int -> Node Expr
  NBinOp :: BinOp -> Hash Expr -> Hash Expr -> Node Expr
  NUnOp :: UnOp -> Hash Expr -> Node Expr
  NEIdent :: String -> Node Expr

  NBlock :: NonEmpty (Hash Statement) -> Node Block

  NIdent :: String -> Node Ident

  NList :: NodeType a -> [Hash a] -> Node (List a)

  NSHole :: Node Statement
  NEHole :: Node Expr
  NIHole :: Node Ident
deriving instance Show (Node a)
instance GEq Node where
  geq a b =
    case a of
      NList nt hs ->
        case b of
          NList nt' hs' -> do
            Refl <- geq nt nt'
            if hs == hs' then Just Refl else Nothing
          _ -> Nothing
      NFor ha hb hc ->
        case b of
          NFor ha' hb' hc' -> do
            Refl <- geq ha ha'
            Refl <- geq hb hb'
            Refl <- geq hc hc'
            pure Refl
          _ -> Nothing
      NIfThen ha hb ->
        case b of
          NIfThen ha' hb' -> do
            Refl <- geq ha ha'
            Refl <- geq hb hb'
            pure Refl
          _ -> Nothing
      NIfThenElse ha hb hc ->
        case b of
          NIfThenElse ha' hb' hc' -> do
            Refl <- geq ha ha'
            Refl <- geq hb hb'
            Refl <- geq hc hc'
            pure Refl
          _ -> Nothing
      NPrint ha ->
        case b of
          NPrint ha' -> do
            Refl <- geq ha ha'
            pure Refl
          _ -> Nothing
      NReturn ha ->
        case b of
          NReturn ha' -> do
            Refl <- geq ha ha'
            pure Refl
          _ -> Nothing
      NDef ha hb hc ->
        case b of
          NDef ha' hb' hc' -> do
            Refl <- geq ha ha'
            Refl <- geq hb hb'
            Refl <- geq hc hc'
            pure Refl
          _ -> Nothing
      NBool x ->
        case b of
          NBool x' -> if x == x' then pure Refl else Nothing
          _ -> Nothing
      NInt x ->
        case b of
          NInt x' -> if x == x' then pure Refl else Nothing
          _ -> Nothing
      NBinOp op ha hb ->
        case b of
          NBinOp op' ha' hb' ->
            if op == op'
            then do
              Refl <- geq ha ha'
              Refl <- geq hb hb'
              pure Refl
            else Nothing
          _ -> Nothing
      NUnOp op ha ->
        case b of
          NUnOp op' ha' ->
            if op == op'
            then do
              Refl <- geq ha ha'
              pure Refl
            else Nothing
          _ -> Nothing
      NEIdent x ->
        case b of
          NEIdent x' ->
            if x == x'
            then pure Refl
            else Nothing
          _ -> Nothing
      NBlock xs ->
        case b of
          NBlock xs' ->
            if xs == xs'
            then pure Refl
            else Nothing
          _ -> Nothing
      NIdent x ->
        case b of
          NIdent x' ->
            if x == x'
            then pure Refl
            else Nothing
          _ -> Nothing
      NSHole ->
        case b of
          NSHole -> pure Refl
          _ -> Nothing
      NEHole ->
        case b of
          NEHole -> pure Refl
          _ -> Nothing
      NIHole ->
        case b of
          NIHole -> pure Refl
          _ -> Nothing
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

      NPrint e -> hashWithSalt s (10::Int, e)
      NDef name args body -> hashWithSalt s (11::Int, name, args, body)

      NIdent i -> hashWithSalt s (12::Int, i)
      NIHole -> hashWithSalt s (13::Int)

      NEIdent i -> hashWithSalt s (13::Int, i)

      NList nt hs -> hashWithSalt s (14::Int, nt, hs)

      NReturn e -> hashWithSalt s (15::Int, e)

hashNode :: forall a. KnownNodeType a => Node a -> Hash a
hashNode n =
  case n of
    NFor{} -> HStatement $ hash n
    NIfThen{} -> HStatement $ hash n
    NIfThenElse{} -> HStatement $ hash n
    NPrint{} -> HStatement $ hash n
    NReturn{} -> HStatement $ hash n
    NDef{} -> HStatement $ hash n

    NBool{} -> HExpr $ hash n
    NInt{} -> HExpr $ hash n
    NBinOp{} -> HExpr $ hash n
    NUnOp{} -> HExpr $ hash n
    NEIdent{} -> HExpr $ hash n

    NBlock{} -> HBlock $ hash n

    NList nt _ -> HList nt $ hash n

    NIdent{} -> HIdent $ hash n

    NSHole -> HStatement $ hash n
    NEHole -> HExpr $ hash n
    NIHole -> HIdent $ hash n
