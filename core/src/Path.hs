{-# language FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# language GADTs, KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
{-# options_ghc -fno-warn-overlapping-patterns #-}
module Path where

import Control.Lens.Fold ((^?))
import Control.Lens.Prism (Prism')
import Control.Lens.Review (review)
import Control.Lens.TH (makePrisms)
import Control.Monad (guard)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Functor.Identity (Identity(..))
import Data.GADT.Compare (GEq(..), GCompare(..), GOrdering(..))
import Data.GADT.Show (GShow(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Type.Equality ((:~:)(..))

import NodeType (KnownNodeType, nodeType)
import qualified NodeType
import Syntax
  ( Expr(..), Statement(..), Block(..), Ident, Args(..), Params(..)
  , _Args, _Params
  )

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

  Return_Value :: Level Statement Expr

  Def_Name :: Level Statement Ident
  Def_Args :: Level Statement Params
  Def_Body :: Level Statement Block

  BinOp_Left :: Level Expr Expr
  BinOp_Right :: Level Expr Expr

  UnOp_Value :: Level Expr Expr

  Call_Function :: Level Expr Expr
  Call_Args :: Level Expr Args

  Block_Index :: Int -> Level Block Statement

  Args_Index :: Int -> Level Args Expr
  Params_Index :: Int -> Level Params Ident
deriving instance Show (Level a b)
makePrisms ''Level
deriveArgDict ''Level

instance GShow (Level a) where
  gshowsPrec = showsPrec

-- type Has' Show (Level a) (Flip Trie f) =
--   (ArgDict (ComposeC Show g) (Level a), ConstraintsFor' (Level a) Show (Flip Trie f)) 

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
    Return_Value ->
      case l2 of
        Return_Value -> Just Refl
        _ -> Nothing
    Def_Name ->
      case l2 of
        Def_Name -> Just Refl
        _ -> Nothing
    Def_Args ->
      case l2 of
        Def_Args -> Just Refl
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
    Call_Function ->
      case l2 of
        Call_Function -> Just Refl
        _ -> Nothing
    Call_Args ->
      case l2 of
        Call_Args -> Just Refl
        _ -> Nothing
    Block_Index n ->
      case l2 of
        Block_Index n' -> do
          guard $ n == n'
          pure Refl
        _ -> Nothing
    Args_Index n ->
      case l2 of
        Args_Index n' -> do
          guard $ n == n'
          pure Refl
        _ -> Nothing
    Params_Index n ->
      case l2 of
        Params_Index n' -> do
          guard $ n == n'
          pure Refl
        _ -> Nothing

instance GEq (Level a) where
  geq = eqLevel

instance GCompare (Level a) where
  gcompare For_Ident For_Ident = GEQ
  gcompare For_Ident _ = GLT
  gcompare _ For_Ident = GGT

  gcompare For_Expr For_Expr = GEQ
  gcompare For_Expr _ = GLT
  gcompare _ For_Expr = GGT

  gcompare For_Expr For_Expr = GEQ
  gcompare For_Expr _ = GLT
  gcompare _ For_Expr = GGT

  gcompare For_Block For_Block = GEQ
  gcompare For_Block _ = GLT
  gcompare _ For_Block = GGT

  gcompare IfThen_Cond IfThen_Cond = GEQ
  gcompare IfThen_Cond _ = GLT
  gcompare _ IfThen_Cond = GGT

  gcompare IfThen_Then IfThen_Then = GEQ
  gcompare IfThen_Then _ = GLT
  gcompare _ IfThen_Then = GGT

  gcompare IfThenElse_Cond IfThenElse_Cond = GEQ
  gcompare IfThenElse_Cond _ = GLT
  gcompare _ IfThenElse_Cond = GGT

  gcompare IfThenElse_Then IfThenElse_Then = GEQ
  gcompare IfThenElse_Then _ = GLT
  gcompare _ IfThenElse_Then = GGT

  gcompare IfThenElse_Else IfThenElse_Else = GEQ
  gcompare IfThenElse_Else _ = GLT
  gcompare _ IfThenElse_Else = GGT

  gcompare Print_Value Print_Value = GEQ
  gcompare Print_Value _ = GLT
  gcompare _ Print_Value = GGT

  gcompare Return_Value Return_Value = GEQ
  gcompare Return_Value _ = GLT
  gcompare _ Return_Value = GGT

  gcompare Def_Name Def_Name = GEQ
  gcompare Def_Name _ = GLT
  gcompare _ Def_Name = GGT

  gcompare Def_Args Def_Args = GEQ
  gcompare Def_Args _ = GLT
  gcompare _ Def_Args = GGT

  gcompare Def_Body Def_Body = GEQ
  gcompare Def_Body _ = GLT
  gcompare _ Def_Body = GGT

  gcompare BinOp_Left BinOp_Left = GEQ
  gcompare BinOp_Left _ = GLT
  gcompare _ BinOp_Left = GGT

  gcompare BinOp_Right BinOp_Right = GEQ
  gcompare BinOp_Right _ = GLT
  gcompare _ BinOp_Right = GGT

  gcompare UnOp_Value UnOp_Value = GEQ
  gcompare UnOp_Value _ = GLT
  gcompare _ UnOp_Value = GGT

  gcompare Call_Function Call_Function = GEQ
  gcompare Call_Function _ = GLT
  gcompare _ Call_Function = GGT

  gcompare Call_Args Call_Args = GEQ
  gcompare Call_Args _ = GLT
  gcompare _ Call_Args = GGT

  gcompare (Block_Index ix) (Block_Index ix') =
    case compare ix ix' of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT
  gcompare Block_Index{} _ = GLT
  gcompare _ Block_Index{} = GGT

  gcompare (Args_Index ix) (Args_Index ix') =
    case compare ix ix' of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT
  gcompare Args_Index{} _ = GLT
  gcompare _ Args_Index{} = GGT

  gcompare (Params_Index ix) (Params_Index ix') =
    case compare ix ix' of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT
  gcompare Params_Index{} _ = GLT
  gcompare _ Params_Index{} = GGT

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

        Return_Value ->
          case a of
            Return val ->
              Return <$>
              traversal p' f val
            _ -> pure a

        Def_Name ->
          case a of
            Def name args body ->
              (\name' -> Def name' args body) <$>
              traversal p' f name
            _ -> pure a
        Def_Args ->
          case a of
            Def name args body ->
              (\args' -> Def name args' body) <$>
              traversal p' f args
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

        Call_Function ->
          case a of
            Call func args ->
              (\func' -> Call func' args) <$>
              traversal p' f func
            _ -> pure a
        Call_Args ->
          case a of
            Call func args ->
              (\args' -> Call func args') <$>
              traversal p' f args
            _ -> pure a

        Block_Index n ->
          case a of
            Block sts | n >= 0, n < length sts ->
              (\val ->
                 let
                   (prefix, suffix) = splitAt n $ NonEmpty.toList sts
                 in
                   Block $ foldr NonEmpty.cons (val NonEmpty.:| drop 1 suffix) prefix
              ) <$>
              traversal p' f (NonEmpty.toList sts !! n)
            _ -> pure a

        Args_Index n ->
          traversalList _Args n p' f a

        Params_Index n ->
          traversalList _Params n p' f a

traversalList ::
  Prism' a [b] ->
  Int ->
  Path b c ->
  forall f. Applicative f => (c -> f c) -> a -> f a
traversalList _Ctor ix path f a =
  case a ^? _Ctor of
    Just xs | ix >= 0, ix < length xs ->
      (\val ->
         let
           (prefix, suffix) = splitAt ix xs
         in
           review _Ctor $ prefix ++ val : drop 1 suffix
      ) <$>
      traversal path f (xs !! ix)
    _ ->
      pure a

modify :: Path a b -> (b -> b) -> a -> a
modify p f = runIdentity . traversal p (Identity . f)

set :: Path a b -> b -> a -> a
set p v = modify p (const v)

withKnownLevelTarget :: forall a b r. KnownNodeType a => Level a b -> (KnownNodeType b => r) -> r
withKnownLevelTarget l k =
  case l of
    For_Ident -> k
    For_Expr -> k
    For_Block -> k
    IfThen_Cond -> k
    IfThen_Then -> k
    IfThenElse_Cond -> k
    IfThenElse_Then -> k
    IfThenElse_Else -> k
    Print_Value -> k
    Return_Value -> k
    Def_Name -> k
    Def_Args -> k
    Def_Body -> k
    BinOp_Left -> k
    BinOp_Right -> k
    UnOp_Value -> k
    Call_Function -> k
    Call_Args -> k
    Block_Index{} -> k
    Args_Index{} -> k
    Params_Index{} -> k

showingPathTarget :: forall a b r. KnownNodeType a => Path a b -> (Show b => r) -> r
showingPathTarget path f =
  case path of
    Nil ->
      NodeType.showingNodeType (nodeType @a) f
    Cons l rest ->
      withKnownLevelTarget l (showingPathTarget rest f)
