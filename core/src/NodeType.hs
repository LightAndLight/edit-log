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

import Syntax (Block, Expr, List, Statement, Ident)

data NodeType :: * -> * where
  TExpr :: NodeType Expr
  TStatement :: NodeType Statement
  TBlock :: NodeType Block
  TIdent :: NodeType Ident
  TList :: NodeType a -> NodeType (List a)
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
      TList nt' -> hashWithSalt s (4::Int, nt')

eqNodeType :: NodeType a -> NodeType b -> Maybe (a :~: b)
eqNodeType = geq

class KnownNodeType t where; nodeType :: NodeType t

instance KnownNodeType Expr where; nodeType = TExpr
instance KnownNodeType Statement where; nodeType = TStatement
instance KnownNodeType Block where; nodeType = TBlock
instance KnownNodeType Ident where; nodeType = TIdent
instance KnownNodeType a => KnownNodeType (List a) where; nodeType = TList nodeType

withNodeType :: NodeType a -> (KnownNodeType a => r) -> r
withNodeType n k =
  case n of
    TExpr -> k
    TStatement -> k
    TBlock -> k
    TIdent -> k
    TList n' -> withNodeType n' k

showingNodeType :: NodeType a -> (Show a => r) -> r
showingNodeType nt k =
  case nt of
    TExpr -> k
    TStatement -> k
    TBlock -> k
    TIdent -> k
    TList nt' -> showingNodeType nt' k
