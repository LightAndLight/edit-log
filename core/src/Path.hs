{-# language ConstraintKinds #-}
{-# language FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# language GADTs, KindSignatures #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# options_ghc -fno-warn-overlapping-patterns #-}
module Path where

import Control.Lens.TH (makePrisms)
import Control.Monad (guard)
import Data.Constraint (Dict(..))
import Data.Constraint.Extras (ArgDict(..), has)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Functor.Identity (Identity(..))
import Data.GADT.Compare (GEq(..), GCompare(..), GOrdering(..))
import Data.GADT.Show (GShow(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Type.Equality ((:~:)(..))

import Hash (Hash)
import Node (Node(..))
import NodeType (KnownNodeType)
import Syntax
  ( Expr(..), Statement(..), Block(..), Ident, Args(..), Params(..)
  , Exprs(..)
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

  List_Exprs :: Level Expr Exprs

  Exprs_Index :: Int -> Level Exprs Expr

  Block_Index :: Int -> Level Block Statement

  Args_Index :: Int -> Level Args Expr
  Params_Index :: Int -> Level Params Ident
deriving instance Show (Level a b)
makePrisms ''Level
deriveArgDict ''Level

instance GShow (Level a) where
  gshowsPrec = showsPrec

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
    List_Exprs ->
      case l2 of
        List_Exprs -> Just Refl
        _ -> Nothing
    Exprs_Index n ->
      case l2 of
        Exprs_Index n' -> do
          guard $ n == n'
          pure Refl
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

-- | This GCompare instance should be in 'syntactic order', where
-- one level is `LT` another if that position occurs earlier in the syntax tree.
-- Used for finding next/prev errors
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

  gcompare List_Exprs List_Exprs = GEQ
  gcompare List_Exprs _ = GLT
  gcompare _ List_Exprs = GGT

  gcompare (Exprs_Index ix) (Exprs_Index ix') =
    case compare ix ix' of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT
  gcompare Exprs_Index{} _ = GLT
  gcompare _ Exprs_Index{} = GGT

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

downLevel :: Level a b -> a -> Maybe (b, b -> a)
downLevel level a =
  case level of
    For_Ident ->
      case a of
        For ident expr body -> Just (ident, \ident' -> For ident' expr body)
        _ -> Nothing
    For_Expr ->
      case a of
        For ident expr body -> Just (expr, \expr' -> For ident expr' body)
        _ -> Nothing
    For_Block ->
      case a of
        For ident expr body -> Just (body, \body' -> For ident expr body')
        _ -> Nothing
    IfThen_Cond ->
      case a of
        IfThen cond then_ -> Just (cond, \cond' -> IfThen cond' then_)
        _ -> Nothing
    IfThen_Then ->
      case a of
        IfThen cond then_ -> Just (then_, \then_' -> IfThen cond then_')
        _ -> Nothing
    IfThenElse_Cond ->
      case a of
        IfThenElse cond then_ else_ -> Just (cond, \cond' -> IfThenElse cond' then_ else_)
        _ -> Nothing
    IfThenElse_Then ->
      case a of
        IfThenElse cond then_ else_ -> Just (then_, \then_' -> IfThenElse cond then_' else_)
        _ -> Nothing
    IfThenElse_Else ->
      case a of
        IfThenElse cond then_ else_ -> Just (else_, \else_' -> IfThenElse cond then_ else_')
        _ -> Nothing
    Print_Value ->
      case a of
        Print val -> Just (val, \val' -> Print val')
        _ -> Nothing
    Return_Value ->
      case a of
        Return val -> Just (val, \val' -> Return val')
        _ -> Nothing
    Def_Name ->
      case a of
        Def name params body -> Just (name, \name' -> Def name' params body)
        _ -> Nothing
    Def_Args ->
      case a of
        Def name params body -> Just (params, \params' -> Def name params' body)
        _ -> Nothing
    Def_Body ->
      case a of
        Def name params body -> Just (body, \body' -> Def name params body')
        _ -> Nothing
    BinOp_Left ->
      case a of
        BinOp op left right -> Just (left, \left' -> BinOp op left' right)
        _ -> Nothing
    BinOp_Right ->
      case a of
        BinOp op left right -> Just (right, \right' -> BinOp op left right')
        _ -> Nothing
    UnOp_Value ->
      case a of
        UnOp op val -> Just (val, \val' -> UnOp op val')
        _ -> Nothing
    Call_Function ->
      case a of
        Call func args -> Just (func, \func' -> Call func' args)
        _ -> Nothing
    Call_Args ->
      case a of
        Call func args -> Just (args, \args' -> Call func args')
        _ -> Nothing
    List_Exprs ->
      case a of
        List xs -> Just (xs, \xs' -> List xs')
        _ -> Nothing
    Exprs_Index ix ->
      case a of
        Exprs xs | 0 <= ix && ix < length xs ->
          let
            (prefix, suffix) = splitAt ix xs
          in
            case suffix of
              [] -> error "impossible"
              x : rest ->
                Just (x, \x' -> Exprs $ prefix ++ x' : rest)
        _ -> Nothing
    Block_Index ix ->
      case a of
        Block sts | 0 <= ix && ix < length sts ->
          let
            (prefix, suffix) = splitAt ix $ NonEmpty.toList sts
          in
            case suffix of
              [] -> error "impossible"
              st : rest ->
                Just (st, \st' -> Block $ foldr NonEmpty.cons (st' NonEmpty.:| rest) prefix)
        _ -> Nothing
    Args_Index ix ->
      case a of
        Args xs | 0 <= ix && ix < length xs ->
          let
            (prefix, suffix) = splitAt ix xs
          in
            case suffix of
              [] -> error "impossible"
              x : rest ->
                Just (x, \x' -> Args $ prefix ++ x' : rest)
        _ -> Nothing
    Params_Index ix ->
      case a of
        Params xs ->
          let
            (prefix, suffix) = splitAt ix xs
          in
            case suffix of
              [] -> error "impossible"
              x : rest ->
                Just (x, \x' -> Params $ prefix ++ x' : rest)
        _ -> Nothing

downLevelNode :: Level a b -> Node a -> Maybe (Hash b, Hash b -> Node a)
downLevelNode level a =
  case level of
    For_Ident ->
      case a of
        NFor ident expr body -> Just (ident, \ident' -> NFor ident' expr body)
        _ -> Nothing
    For_Expr ->
      case a of
        NFor ident expr body -> Just (expr, \expr' -> NFor ident expr' body)
        _ -> Nothing
    For_Block ->
      case a of
        NFor ident expr body -> Just (body, \body' -> NFor ident expr body')
        _ -> Nothing
    IfThen_Cond ->
      case a of
        NIfThen cond then_ -> Just (cond, \cond' -> NIfThen cond' then_)
        _ -> Nothing
    IfThen_Then ->
      case a of
        NIfThen cond then_ -> Just (then_, \then_' -> NIfThen cond then_')
        _ -> Nothing
    IfThenElse_Cond ->
      case a of
        NIfThenElse cond then_ else_ -> Just (cond, \cond' -> NIfThenElse cond' then_ else_)
        _ -> Nothing
    IfThenElse_Then ->
      case a of
        NIfThenElse cond then_ else_ -> Just (then_, \then_' -> NIfThenElse cond then_' else_)
        _ -> Nothing
    IfThenElse_Else ->
      case a of
        NIfThenElse cond then_ else_ -> Just (else_, \else_' -> NIfThenElse cond then_ else_')
        _ -> Nothing
    Print_Value ->
      case a of
        NPrint val -> Just (val, \val' -> NPrint val')
        _ -> Nothing
    Return_Value ->
      case a of
        NReturn val -> Just (val, \val' -> NReturn val')
        _ -> Nothing
    Def_Name ->
      case a of
        NDef name params body -> Just (name, \name' -> NDef name' params body)
        _ -> Nothing
    Def_Args ->
      case a of
        NDef name params body -> Just (params, \params' -> NDef name params' body)
        _ -> Nothing
    Def_Body ->
      case a of
        NDef name params body -> Just (body, \body' -> NDef name params body')
        _ -> Nothing
    BinOp_Left ->
      case a of
        NBinOp op left right -> Just (left, \left' -> NBinOp op left' right)
        _ -> Nothing
    BinOp_Right ->
      case a of
        NBinOp op left right -> Just (right, \right' -> NBinOp op left right')
        _ -> Nothing
    UnOp_Value ->
      case a of
        NUnOp op val -> Just (val, \val' -> NUnOp op val')
        _ -> Nothing
    Call_Function ->
      case a of
        NCall func args -> Just (func, \func' -> NCall func' args)
        _ -> Nothing
    Call_Args ->
      case a of
        NCall func args -> Just (args, \args' -> NCall func args')
        _ -> Nothing
    List_Exprs ->
      case a of
        NList xs -> Just (xs, \xs' -> NList xs')
        _ -> Nothing
    Exprs_Index ix ->
      case a of
        NExprs xs | 0 <= ix && ix < length xs ->
          let
            (prefix, suffix) = splitAt ix xs
          in
            case suffix of
              [] -> error "impossible"
              x : rest ->
                Just (x, \x' -> NExprs $ prefix ++ x' : rest)
        _ -> Nothing
    Block_Index ix ->
      case a of
        NBlock sts | 0 <= ix && ix < length sts ->
          let
            (prefix, suffix) = splitAt ix $ NonEmpty.toList sts
          in
            case suffix of
              [] -> error "impossible"
              st : rest ->
                Just (st, \st' -> NBlock $ foldr NonEmpty.cons (st' NonEmpty.:| rest) prefix)
        _ -> Nothing
    Args_Index ix ->
      case a of
        NArgs xs | 0 <= ix && ix < length xs ->
          let
            (prefix, suffix) = splitAt ix xs
          in
            case suffix of
              [] -> error "impossible"
              x : rest ->
                Just (x, \x' -> NArgs $ prefix ++ x' : rest)
        _ -> Nothing
    Params_Index ix ->
      case a of
        NParams xs ->
          let
            (prefix, suffix) = splitAt ix xs
          in
            case suffix of
              [] -> error "impossible"
              x : rest ->
                Just (x, \x' -> NParams $ prefix ++ x' : rest)
        _ -> Nothing

data Path a :: * -> * where
  Nil :: Path a a
  Cons :: Level a b -> Path b c -> Path a c
deriving instance Show (Path a b)
instance ArgDict c (Path a) where
  type ConstraintsFor (Path a) c = (c a, ConstraintsFor (Level a) c)
  argDict path =
    case path of
      Nil ->
        Dict
      Cons l rest ->
        has @c l (argDict rest)

data SomePath a where
  SomePath :: KnownNodeType b => Path a b -> SomePath a

append :: Path a b -> Path b c -> Path a c
append p1 p2 =
  case p1 of
    Nil -> p2
    Cons l p1' -> Cons l (append p1' p2)

data Split a b where
  Split :: Path a x -> Path x b -> Split a b

splitFromEnd :: Path a b -> Int -> Split a b
splitFromEnd path n =
  if n <= 0
  then Split path Nil
  else
    case unsnoc path of
      UnsnocMore prefix final ->
        case splitFromEnd prefix (n-1) of
          Split prefix' suffix' -> Split prefix' (snoc suffix' final)
      UnsnocEmpty -> Split Nil Nil

snoc :: Path a b -> Level b c -> Path a c
snoc p l =
  case p of
    Nil -> Cons l Nil
    Cons l' p' -> Cons l' (snoc p' l)

data Unsnoc a b where
  UnsnocMore :: Path a x -> Level x b -> Unsnoc a b
  UnsnocEmpty :: Unsnoc a a

unsnoc :: Path a b -> Unsnoc a b
unsnoc path =
  case path of
    Nil -> UnsnocEmpty
    Cons l rest ->
      case unsnoc rest of
        UnsnocEmpty ->
          UnsnocMore Nil l
        UnsnocMore prefix final ->
          UnsnocMore (Cons l prefix) final

instance GEq (Path a) where
  geq = eqPath

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

instance GCompare (Path a) where
  gcompare Nil Nil = GEQ
  gcompare Nil _ = GLT
  gcompare _ Nil = GGT

  gcompare (Cons l ls) (Cons l' ls') =
    case gcompare l l' of
      GEQ ->
        case gcompare ls ls' of
          GEQ -> GEQ
          GLT -> GLT
          GGT -> GGT
      GLT -> GLT
      GGT -> GGT
  gcompare Cons{} _ = GLT
  gcompare _ Cons{} = GGT

traversal :: Path a b -> forall f. Applicative f => (b -> f b) -> a -> f a
traversal p f a =
  case p of
    Nil -> f a
    Cons l p' ->
      case downLevel l a of
        Nothing -> pure a
        Just (a', mk) ->
          mk <$> traversal p' f a'

modify :: Path a b -> (b -> b) -> a -> a
modify p f = runIdentity . traversal p (Identity . f)

set :: Path a b -> b -> a -> a
set p v = modify p (const v)
