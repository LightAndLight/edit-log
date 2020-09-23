{-# language GADTs, KindSignatures #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
module Path where

import Data.Functor.Identity (Identity(..))

import Syntax (Expr(..), Statement(..), Block(..), Ident)

data Path :: * -> * -> * where
  Nil :: Path a a
  Cons :: Level a b -> Path b c -> Path a c
deriving instance Show (Path a b)

data Level :: * -> * -> * where
  For_Ident :: Level Statement Ident
  For_Expr :: Level Statement Expr
  For_Block :: Level Statement Block

  IfThen_Cond :: Level Statement Expr
  IfThen_Then :: Level Statement Block

  IfThenElse_Cond :: Level Statement Expr
  IfThenElse_Then :: Level Statement Block
  IfThenElse_Else :: Level Statement Block

  BinOp_Left :: Level Expr Expr
  BinOp_Right :: Level Expr Expr

  UnOp_Value :: Level Expr Expr

  Block_Index :: Int -> Level Block Statement
deriving instance Show (Level a b)

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
    BinOp_Left -> f
    BinOp_Right -> f
    UnOp_Value -> f
    Block_Index{} -> f

showingPathTarget :: Show a => Path a b -> (Show b => r) -> r
showingPathTarget path f =
  case path of
    Nil -> f
    Cons l rest -> showingLevelTarget l (showingPathTarget rest f)
