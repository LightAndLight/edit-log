{-# language GADTs, KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module NodeType where

import Data.GADT.Compare (geq)
import Data.GADT.Compare.TH (deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Hashable (Hashable(..))
import Data.Type.Equality ((:~:))

import Syntax (Block, Expr, Statement, Ident, Args, Params)

data NodeType :: * -> * where
  TExpr :: NodeType Expr
  TStatement :: NodeType Statement
  TBlock :: NodeType Block
  TIdent :: NodeType Ident
  TArgs :: NodeType Args
  TParams :: NodeType Params
deriveGShow ''NodeType
deriveGEq ''NodeType
deriving instance Show (NodeType a)
instance Hashable (NodeType a) where
  hashWithSalt s nt =
    case nt of
      TExpr -> hashWithSalt s (0::Int)
      TStatement -> hashWithSalt s (1::Int)
      TBlock -> hashWithSalt s (2::Int)
      TIdent -> hashWithSalt s (3::Int)
      TArgs -> hashWithSalt s (4::Int)
      TParams -> hashWithSalt s (5::Int)

eqNodeType :: NodeType a -> NodeType b -> Maybe (a :~: b)
eqNodeType = geq

class KnownNodeType t where; nodeType :: NodeType t

instance KnownNodeType Expr where; nodeType = TExpr
instance KnownNodeType Statement where; nodeType = TStatement
instance KnownNodeType Block where; nodeType = TBlock
instance KnownNodeType Ident where; nodeType = TIdent
instance KnownNodeType Args where; nodeType = TArgs
instance KnownNodeType Params where; nodeType = TParams

withNodeType :: NodeType a -> (KnownNodeType a => r) -> r
withNodeType n k =
  case n of
    TExpr -> k
    TStatement -> k
    TBlock -> k
    TIdent -> k
    TArgs -> k
    TParams -> k

showingNodeType :: NodeType a -> (Show a => r) -> r
showingNodeType nt k =
  case nt of
    TExpr -> k
    TStatement -> k
    TBlock -> k
    TIdent -> k
    TArgs -> k
    TParams -> k

eqingNodeType :: NodeType a -> (Eq a => r) -> r
eqingNodeType nt k =
  case nt of
    TExpr -> k
    TStatement -> k
    TBlock -> k
    TIdent -> k
    TArgs -> k
    TParams -> k
