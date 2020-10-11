{-# language GADTs, KindSignatures #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
module NodeType where

import Data.GADT.Show.TH (deriveGShow)
import Data.Type.Equality ((:~:)(Refl))

import Syntax (Block, Expr, Statement, Ident)

data NodeType :: * -> * where
  TExpr :: NodeType Expr
  TStatement :: NodeType Statement
  TBlock :: NodeType Block
  TIdent :: NodeType Ident
deriveGShow ''NodeType
deriving instance Show (NodeType a)

eqNodeType :: NodeType a -> NodeType b -> Maybe (a :~: b)
eqNodeType ty ty' =
  case ty of
    TBlock ->
      case ty' of
        TBlock -> Just Refl
        _ -> Nothing
    TExpr ->
      case ty' of
        TExpr -> Just Refl
        _ -> Nothing
    TStatement ->
      case ty' of
        TStatement -> Just Refl
        _ -> Nothing
    TIdent ->
      case ty' of
        TIdent -> Just Refl
        _ -> Nothing

class KnownNodeType t where; nodeType :: NodeType t

instance KnownNodeType Expr where; nodeType = TExpr
instance KnownNodeType Statement where; nodeType = TStatement
instance KnownNodeType Block where; nodeType = TBlock
instance KnownNodeType Ident where; nodeType = TIdent
