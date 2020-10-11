{-# language GADTs, KindSignatures #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language TypeOperators #-}
{-# options_ghc -fno-warn-overlapping-patterns #-}
module Path where

import Control.Monad (guard)
import Data.Functor.Identity (Identity(..))
import Data.Type.Equality ((:~:)(..))

import Syntax (Expr(..), Statement(..), Block(..), Ident)

data Path a :: * -> * where
  Nil :: Path a a
  Cons :: Level a b -> Path b c -> Path a c
deriving instance Show (Path a b)

append :: Path a b -> Path b c -> Path a c
append p1 p2 =
  case p1 of
    Nil -> p2
    Cons l p1' -> Cons l (append p1' p2)

snoc :: Path a b -> Level b c -> Path a c
snoc p l =
  case p of
    Nil -> Cons l Nil
    Cons l' p' -> Cons l' (snoc p' l)

eqPath :: Path a b -> Path a c -> Maybe (b :~: c)
eqPath p1 p2 =
  case p1 of
    Nil ->
      case p2 of
        Nil -> Just Refl
        Cons{} -> Nothing
    Cons l p1' ->
      case p2 of
        Nil -> Nothing
        Cons l' p2' -> do
          Refl <- eqLevel l l'
          eqPath p1' p2'

data Level :: * -> * -> * where
  For_Ident :: Level Statement Ident
  For_Expr :: Level Statement Expr
  For_Block :: Level Statement Block

  IfThen_Cond :: Level Statement Expr
  IfThen_Then :: Level Statement Block

  IfThenElse_Cond :: Level Statement Expr
  IfThenElse_Then :: Level Statement Block
  IfThenElse_Else :: Level Statement Block

  Print_Value :: Level Statement Expr

  Def_Name :: Level Statement Ident
  Def_Body :: Level Statement Block

  BinOp_Left :: Level Expr Expr
  BinOp_Right :: Level Expr Expr

  UnOp_Value :: Level Expr Expr

  Block_Index :: Int -> Level Block Statement
deriving instance Show (Level a b)

eqLevel :: Level a b -> Level a c -> Maybe (b :~: c)
eqLevel l1 l2 =
  case l1 of
    For_Ident ->
      case l2 of
        For_Ident -> Just Refl
        _ -> Nothing
    For_Expr ->
      case l2 of
        For_Expr -> Just Refl
        _ -> Nothing
    For_Block ->
      case l2 of
        For_Block -> Just Refl
        _ -> Nothing
    IfThen_Cond ->
      case l2 of
        IfThen_Cond -> Just Refl
        _ -> Nothing
    IfThen_Then ->
      case l2 of
        IfThen_Then -> Just Refl
        _ -> Nothing
    IfThenElse_Cond ->
      case l2 of
        IfThenElse_Cond -> Just Refl
        _ -> Nothing
    IfThenElse_Then ->
      case l2 of
        IfThenElse_Then -> Just Refl
        _ -> Nothing
    IfThenElse_Else ->
      case l2 of
        IfThenElse_Else -> Just Refl
        _ -> Nothing
    Print_Value ->
      case l2 of
        Print_Value -> Just Refl
        _ -> Nothing
    Def_Name ->
      case l2 of
        Def_Name -> Just Refl
        _ -> Nothing
    Def_Body ->
      case l2 of
        Def_Body -> Just Refl
        _ -> Nothing
    BinOp_Left ->
      case l2 of
        BinOp_Left -> Just Refl
        _ -> Nothing
    BinOp_Right ->
      case l2 of
        BinOp_Right -> Just Refl
        _ -> Nothing
    UnOp_Value ->
      case l2 of
        UnOp_Value -> Just Refl
        _ -> Nothing
    Block_Index n ->
      case l2 of
        Block_Index n' -> do
          guard $ n == n'
          pure Refl
        _ -> Nothing

traversal :: Path a b -> forall f. Applicative f => (b -> f b) -> a -> f a
traversal p f a =
  case p of
    Nil -> f a
    Cons l p' ->
      case l of
        For_Ident ->
          case a of
            For ident expr block ->
              (\ident' -> For ident' expr block) <$>
              traversal p' f ident
            _ -> pure a
        For_Expr ->
          case a of
            For ident expr block ->
              (\expr' -> For ident expr' block) <$>
              traversal p' f expr
            _ -> pure a
        For_Block ->
          case a of
            For ident expr block ->
              (\block' -> For ident expr block') <$>
              traversal p' f block
            _ -> pure a

        IfThen_Cond ->
          case a of
            IfThen cond then_ ->
              (\cond' -> IfThen cond' then_) <$>
              traversal p' f cond
            _ -> pure a
        IfThen_Then ->
          case a of
            IfThen cond then_ ->
              (\then_' -> IfThen cond then_') <$>
              traversal p' f then_
            _ -> pure a

        IfThenElse_Cond ->
          case a of
            IfThenElse cond then_ else_ ->
              (\cond' -> IfThenElse cond' then_ else_) <$>
              traversal p' f cond
            _ -> pure a
        IfThenElse_Then ->
          case a of
            IfThenElse cond then_ else_ ->
              (\then_' -> IfThenElse cond then_' else_) <$>
              traversal p' f then_
            _ -> pure a
        IfThenElse_Else ->
          case a of
            IfThenElse cond then_ else_ ->
              (\else_' -> IfThenElse cond then_ else_') <$>
              traversal p' f else_
            _ -> pure a

        Print_Value ->
          case a of
            Print val ->
              Print <$>
              traversal p' f val
            _ -> pure a

        Def_Name ->
          case a of
            Def name args body ->
              (\name' -> Def name' args body) <$>
              traversal p' f name
            _ -> pure a
        Def_Body ->
          case a of
            Def name args body ->
              Def name args <$>
              traversal p' f body
            _ -> pure a

        BinOp_Left ->
          case a of
            BinOp op left right ->
              (\left' -> BinOp op left' right) <$>
              traversal p' f left
            _ -> pure a
        BinOp_Right ->
          case a of
            BinOp op left right ->
              (\right' -> BinOp op left right') <$>
              traversal p' f right
            _ -> pure a

        UnOp_Value ->
          case a of
            UnOp op value ->
              (\value' -> UnOp op value') <$>
              traversal p' f value
            _ -> pure a

        Block_Index n ->
          case a of
            Block sts | n >= 0, n < length sts ->
              (\val -> let (prefix, suffix) = splitAt n sts in Block $ prefix ++ val : drop 1 suffix) <$>
              traversal p' f (sts !! n)
            _ -> pure a

modify :: Path a b -> (b -> b) -> a -> a
modify p f = runIdentity . traversal p (Identity . f)

set :: Path a b -> b -> a -> a
set p v = modify p (const v)

showingLevelTarget :: Show a => Level a b -> (Show b => r) -> r
showingLevelTarget l f =
  case l of
    For_Ident -> f
    For_Expr -> f
    For_Block -> f
    IfThen_Cond -> f
    IfThen_Then -> f
    IfThenElse_Cond -> f
    IfThenElse_Then -> f
    IfThenElse_Else -> f
    Print_Value -> f
    Def_Name -> f
    Def_Body -> f
    BinOp_Left -> f
    BinOp_Right -> f
    UnOp_Value -> f
    Block_Index{} -> f

showingPathTarget :: Show a => Path a b -> (Show b => r) -> r
showingPathTarget path f =
  case path of
    Nil -> f
    Cons l rest -> showingLevelTarget l (showingPathTarget rest f)
