{-# language GADTs, KindSignatures, ScopedTypeVariables, TypeApplications #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
module Node where

import Control.Monad (guard)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable(..))
import Data.Type.Equality ((:~:)(Refl))

import Hash (Hash(..), eqHash)
import Syntax (Expr, Statement, Block, UnOp, BinOp, Ident)

data NodeType :: * -> * where
  TExpr :: NodeType Expr
  TStatement :: NodeType Statement
  TBlock :: NodeType Block

class KnownNodeType t where; nodeType :: NodeType t

instance KnownNodeType Expr where; nodeType = TExpr
instance KnownNodeType Statement where; nodeType = TStatement
instance KnownNodeType Block where; nodeType = TBlock
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

eqNode :: Node a -> Node b -> Maybe (a :~: b)
eqNode n1 n2 =
  case n1 of
    NFor ident expr body ->
      case n2 of
        NFor ident' expr' body' -> do
          guard $ ident == ident'
          Refl <- expr `eqHash` expr'
          Refl <- body `eqHash` body'
          pure Refl
        _ -> Nothing
    NIfThen cond then_ ->
      case n2 of
        NIfThen cond' then_' -> do
          Refl <- cond `eqHash` cond'
          Refl <- then_ `eqHash` then_'
          pure Refl
        _ -> Nothing
    NIfThenElse cond then_ else_ ->
      case n2 of
        NIfThenElse cond' then_' else_' -> do
          Refl <- cond `eqHash` cond'
          Refl <- then_ `eqHash` then_'
          Refl <- else_ `eqHash` else_'
          pure Refl
        _ -> Nothing
    NBool b ->
      case n2 of
        NBool b' -> do
          guard $ b == b'
          pure Refl
        _ -> Nothing
    NInt v ->
      case n2 of
        NInt v' -> do
          guard $ v == v'
          pure Refl
        _ -> Nothing
    NBinOp op l r ->
      case n2 of
        NBinOp op' l' r' -> do
          guard $ op == op'
          Refl <- l `eqHash` l'
          Refl <- r `eqHash` r'
          pure Refl
        _ -> Nothing
    NUnOp op value ->
      case n2 of
        NUnOp op' value' -> do
          guard $ op == op'
          Refl <- value `eqHash` value'
          pure Refl
        _ -> Nothing
    NBlock sts ->
      case n2 of
        NBlock sts' -> do
          traverse_ (uncurry eqHash) (zip sts sts')
          pure Refl
        _ -> Nothing
    NSHole ->
      case n2 of
        NSHole -> Just Refl
        _ -> Nothing
    NEHole ->
      case n2 of
        NEHole -> Just Refl
        _ -> Nothing

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
