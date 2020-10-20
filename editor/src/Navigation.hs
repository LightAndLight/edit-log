{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
module Navigation
  ( findNextHole
  , nextHole
  , findPrevHole
  , prevHole
  )
where

import Control.Applicative ((<|>))
import Control.Lens.Fold ((^?))
import Control.Lens.Prism (Prism')
import Control.Lens.Review (review)
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Alt(..))
import qualified Data.List.NonEmpty as NonEmpty

import Hash (Hash)
import Node (Node(..), _NArgs, _NExprs, _NParams)
import Path (Path(..), Level(..), _Args_Index, _Exprs_Index, _Params_Index)
import qualified Path
import qualified Store
import qualified Versioned
import Versioned.Pure (Versioned, runVersionedT)

import Focus (Focus(..))

data SearchEntry a where
  SearchEntry :: Path a x -> Hash x -> SearchEntry a
deriving instance Show (SearchEntry a)

findNextHole :: Versioned a -> Path x y -> Hash y -> Maybe (Focus x)
findNextHole v = go
  where
    go :: Path b c -> Hash c -> Maybe (Focus b)
    go path h = do
      let Identity (mNode, _) = runVersionedT v $ Store.lookupNode h
      node <- mNode
      case node of
        NFor ident expr body ->
          go (Path.snoc path For_Ident) ident <|>
          go (Path.snoc path For_Expr) expr <|>
          go (Path.snoc path For_Block) body
        NIfThen cond then_ ->
          go (Path.snoc path IfThen_Cond) cond <|>
          go (Path.snoc path IfThen_Then) then_
        NIfThenElse cond then_ else_ ->
          go (Path.snoc path IfThenElse_Cond) cond <|>
          go (Path.snoc path IfThenElse_Then) then_ <|>
          go (Path.snoc path IfThenElse_Else) else_
        NPrint val ->
          go (Path.snoc path Print_Value) val
        NReturn val ->
          go (Path.snoc path Return_Value) val
        NDef name args body ->
          go (Path.snoc path Def_Name) name <|>
          go (Path.snoc path Def_Args) args <|>
          go (Path.snoc path Def_Body) body
        NBool{} -> Nothing
        NInt{} -> Nothing
        NEIdent{} -> Nothing
        NBinOp _ left right ->
          go (Path.snoc path BinOp_Left) left <|>
          go (Path.snoc path BinOp_Right) right
        NUnOp _ val ->
          go (Path.snoc path UnOp_Value) val
        NCall func args ->
          go (Path.snoc path Call_Function) func <|>
          go (Path.snoc path Call_Args) args
        NList xs ->
          go (Path.snoc path List_Exprs) xs
        NExprs xs -> findNextHoleList _Exprs_Index v path xs
        NBlock sts ->
          getAlt $
          foldMap
            (\(ix, st) -> Alt $ go (Path.snoc path $ Block_Index ix) st)
            (zip [0..] $ NonEmpty.toList sts)
        NIdent{} -> Nothing
        NSHole -> Just $ Focus path
        NEHole -> Just $ Focus path
        NIHole -> Just $ Focus path
        NArgs xs -> findNextHoleList _Args_Index v path xs
        NParams xs -> findNextHoleList _Params_Index v path xs

findNextHoleList ::
  Prism' (Level y z) Int ->
  Versioned a ->
  Path x y ->
  [Hash z] ->
  Maybe (Focus x)
findNextHoleList _Level versioned path xs =
  getAlt $
  foldMap
    (\(ix, x) -> Alt $ findNextHole versioned (Path.snoc path $ review _Level ix) x)
    (zip [0..] xs)

searchTreeForward :: forall a x y. Versioned a -> Path a x -> Path x y -> Hash x -> [SearchEntry a]
searchTreeForward v context path h =
  let Identity (mNode, _) = runVersionedT v $ Store.lookupNode h in
  case path of
    Nil ->
      case mNode of
        Nothing -> []
        Just node ->
          case node of
            NSHole -> []
            NEHole -> []
            NIHole -> []
            _ -> [SearchEntry context h]
    Cons l path' -> do
      case mNode of
        Nothing -> []
        Just node ->
          case l of
            For_Ident ->
              case node of
                NFor ident expr body ->
                  searchTreeForward v (Path.snoc context For_Ident) path' ident <>
                  [ SearchEntry (Path.snoc context For_Expr) expr
                  , SearchEntry (Path.snoc context For_Block) body
                  ]
                _ -> []
            For_Expr ->
              case node of
                NFor _ expr body ->
                  searchTreeForward v (Path.snoc context For_Expr) path' expr <>
                  [ SearchEntry (Path.snoc context For_Block) body ]
                _ -> []
            For_Block ->
              case node of
                NFor _ _ body ->
                  searchTreeForward v (Path.snoc context For_Block) path' body
                _ -> []
            IfThen_Cond ->
              case node of
                NIfThen cond then_ ->
                  searchTreeForward v (Path.snoc context IfThen_Cond) path' cond <>
                  [ SearchEntry (Path.snoc context IfThen_Then) then_ ]
                _ -> []
            IfThen_Then ->
              case node of
                NIfThen _ then_ ->
                  searchTreeForward v (Path.snoc context IfThen_Then) path' then_
                _ -> []
            IfThenElse_Cond ->
              case node of
                NIfThenElse cond then_ else_->
                  searchTreeForward v (Path.snoc context IfThenElse_Cond) path' cond <>
                  [ SearchEntry (Path.snoc context IfThenElse_Then) then_
                  , SearchEntry (Path.snoc context IfThenElse_Else) else_
                  ]
                _ -> []
            IfThenElse_Then ->
              case node of
                NIfThenElse _ then_ else_->
                  searchTreeForward v (Path.snoc context IfThenElse_Then) path' then_ <>
                  [ SearchEntry (Path.snoc context IfThenElse_Else) else_ ]
                _ -> []
            IfThenElse_Else ->
              case node of
                NIfThenElse _ _ else_->
                  searchTreeForward v (Path.snoc context IfThenElse_Else) path' else_
                _ -> []
            Print_Value ->
              case node of
                NPrint val ->
                  searchTreeForward v (Path.snoc context Print_Value) path' val
                _ -> []
            Return_Value ->
              case node of
                NReturn val ->
                  searchTreeForward v (Path.snoc context Return_Value) path' val
                _ -> []
            Def_Name ->
              case node of
                NDef name args body ->
                  searchTreeForward v (Path.snoc context Def_Name) path' name <>
                  [ SearchEntry (Path.snoc context Def_Args) args
                  , SearchEntry (Path.snoc context Def_Body) body
                  ]
                _ -> []
            Def_Args ->
              case node of
                NDef _ args body ->
                  searchTreeForward v (Path.snoc context Def_Args) path' args <>
                  [ SearchEntry (Path.snoc context Def_Body) body ]
                _ -> []
            Def_Body ->
              case node of
                NDef _ _ body ->
                  searchTreeForward v (Path.snoc context Def_Body) path' body
                _ -> []
            BinOp_Left ->
              case node of
                NBinOp _ left right->
                  searchTreeForward v (Path.snoc context BinOp_Left) path' left <>
                  [ SearchEntry (Path.snoc context BinOp_Right) right ]
                _ -> []
            BinOp_Right ->
              case node of
                NBinOp _ _ right->
                  searchTreeForward v (Path.snoc context BinOp_Right) path' right
                _ -> []
            UnOp_Value ->
              case node of
                NUnOp _ val ->
                  searchTreeForward v (Path.snoc context UnOp_Value) path' val
                _ -> []
            Call_Function ->
              case node of
                NCall func args->
                  searchTreeForward v (Path.snoc context Call_Function) path' func <>
                  [ SearchEntry (Path.snoc context Call_Args) args ]
                _ -> []
            Call_Args ->
              case node of
                NCall _ args->
                  searchTreeForward v (Path.snoc context Call_Args) path' args
                _ -> []
            List_Exprs ->
              case node of
                NList exprs->
                  searchTreeForward v (Path.snoc context List_Exprs) path' exprs
                _ -> []
            Exprs_Index ix ->
              searchTreeForwardList _NExprs _Exprs_Index ix v context path' node
            Block_Index ix ->
              case node of
                NBlock sts ->
                  let
                    (_, suffix) = splitAt ix . zip [0..] $ NonEmpty.toList sts
                  in
                    case suffix of
                      (_, st) : rest ->
                        searchTreeForward v (Path.snoc context $ Block_Index ix) path' st <>
                        fmap (\(ix', st') -> SearchEntry (Path.snoc context $ Block_Index ix') st') rest
                      [] -> []
            Args_Index ix ->
              searchTreeForwardList _NArgs _Args_Index ix v context path' node
            Params_Index ix ->
              searchTreeForwardList _NParams _Params_Index ix v context path' node

searchTreeForwardList ::
  forall a x y z.
  Prism' (Node x) [Hash y] ->
  Prism' (Level x y) Int ->
  Int ->
  Versioned a ->
  Path a x ->
  Path y z ->
  Node x ->
  [SearchEntry a]
searchTreeForwardList _Ctor _Level ix v context path node =
  case node ^? _Ctor of
    Just xs ->
      let
        (_, suffix) = splitAt ix $ zip [0..] xs
      in
        case suffix of
          (_, x) : rest ->
            searchTreeForward v (Path.snoc context $ review _Level ix) path x <>
            fmap (\(ix', x') -> SearchEntry (Path.snoc context $ review _Level ix') x') rest
          [] -> []
    Nothing -> []

nextHole :: forall a b. Versioned a -> Path a b -> Maybe (Focus a)
nextHole v focusPath = do
  let Identity (h, _) = runVersionedT v Versioned.getRoot
  let tree = searchTreeForward v Nil focusPath h
  getAlt $ foldMap (\(SearchEntry sPath sH) -> Alt $ findNextHole v sPath sH) tree

findPrevHole :: Versioned a -> Path x y -> Hash y -> Maybe (Focus x)
findPrevHole v = go
  where
    go :: Path b c -> Hash c -> Maybe (Focus b)
    go path h = do
      let Identity (mNode, _) = runVersionedT v $ Store.lookupNode h
      node <- mNode
      case node of
        NFor ident expr body ->
          go (Path.snoc path For_Ident) ident <|>
          go (Path.snoc path For_Block) body <|>
          go (Path.snoc path For_Expr) expr
        NIfThen cond then_ ->
          go (Path.snoc path IfThen_Then) then_ <|>
          go (Path.snoc path IfThen_Cond) cond
        NIfThenElse cond then_ else_ ->
          go (Path.snoc path IfThenElse_Else) else_ <|>
          go (Path.snoc path IfThenElse_Then) then_ <|>
          go (Path.snoc path IfThenElse_Cond) cond
        NPrint val ->
          go (Path.snoc path Print_Value) val
        NReturn val ->
          go (Path.snoc path Return_Value) val
        NDef name args body ->
          go (Path.snoc path Def_Name) name <|>
          go (Path.snoc path Def_Args) args <|>
          go (Path.snoc path Def_Body) body
        NBool{} -> Nothing
        NInt{} -> Nothing
        NEIdent{} -> Nothing
        NBinOp _ left right ->
          go (Path.snoc path BinOp_Right) right <|>
          go (Path.snoc path BinOp_Left) left
        NUnOp _ val ->
          go (Path.snoc path UnOp_Value) val
        NCall func args ->
          go (Path.snoc path Call_Function) func <|>
          go (Path.snoc path Call_Args) args
        NList exprs ->
          go (Path.snoc path List_Exprs) exprs
        NExprs xs -> findPrevHoleList _Exprs_Index v path xs
        NBlock sts ->
          getAlt $
          foldMap
            (\(ix, st) -> Alt $ go (Path.snoc path $ Block_Index ix) st)
            (reverse . zip [0..] $ NonEmpty.toList sts)
        NArgs xs -> findPrevHoleList _Args_Index v path xs
        NParams xs -> findPrevHoleList _Params_Index v path xs
        NIdent{} -> Nothing
        NSHole -> Just $ Focus path
        NEHole -> Just $ Focus path
        NIHole -> Just $ Focus path

findPrevHoleList ::
  Prism' (Level y z) Int ->
  Versioned a ->
  Path x y ->
  [Hash z] ->
  Maybe (Focus x)
findPrevHoleList _Level versioned path xs =
  getAlt $
  foldMap
    (\(ix, x) -> Alt $ findPrevHole versioned (Path.snoc path $ review _Level ix) x)
    (reverse $ zip [0..] xs)

searchTreeBackward :: forall a x y. Versioned a -> Path a x -> Path x y -> Hash x -> [SearchEntry a]
searchTreeBackward v context path h =
  let Identity (mNode, _) = runVersionedT v $ Store.lookupNode h in
  case path of
    Nil ->
      case mNode of
        Nothing -> []
        Just node ->
          case node of
            NSHole -> []
            NEHole -> []
            _ -> [SearchEntry context h]
    Cons l path' -> do
      case mNode of
        Nothing -> []
        Just node ->
          case l of
            For_Ident ->
              case node of
                NFor ident _ _ ->
                  searchTreeBackward v (Path.snoc context For_Ident) path' ident
                _ -> []
            For_Expr ->
              case node of
                NFor ident expr _ ->
                  searchTreeBackward v (Path.snoc context For_Expr) path' expr <>
                  [ SearchEntry (Path.snoc context For_Ident) ident ]
                _ -> []
            For_Block ->
              case node of
                NFor ident expr body ->
                  searchTreeBackward v (Path.snoc context For_Block) path' body <>
                  [ SearchEntry (Path.snoc context For_Expr) expr
                  , SearchEntry (Path.snoc context For_Ident) ident
                  ]
                _ -> []
            IfThen_Cond ->
              case node of
                NIfThen cond _ ->
                  searchTreeBackward v (Path.snoc context IfThen_Cond) path' cond
                _ -> []
            IfThen_Then ->
              case node of
                NIfThen cond then_ ->
                  searchTreeBackward v (Path.snoc context IfThen_Then) path' then_ <>
                  [ SearchEntry (Path.snoc context IfThen_Cond) cond ]
                _ -> []
            IfThenElse_Cond ->
              case node of
                NIfThenElse cond _ _ ->
                  searchTreeBackward v (Path.snoc context IfThenElse_Cond) path' cond
                _ -> []
            IfThenElse_Then ->
              case node of
                NIfThenElse cond then_ _ ->
                  searchTreeBackward v (Path.snoc context IfThenElse_Then) path' then_ <>
                  [ SearchEntry (Path.snoc context IfThenElse_Cond) cond ]
                _ -> []
            IfThenElse_Else ->
              case node of
                NIfThenElse cond then_ else_->
                  searchTreeBackward v (Path.snoc context IfThenElse_Else) path' else_ <>
                  [ SearchEntry (Path.snoc context IfThenElse_Then) then_
                  , SearchEntry (Path.snoc context IfThenElse_Cond) cond
                  ]
                _ -> []
            Print_Value ->
              case node of
                NPrint val ->
                  searchTreeBackward v (Path.snoc context Print_Value) path' val
                _ -> []
            Return_Value ->
              case node of
                NReturn val ->
                  searchTreeBackward v (Path.snoc context Return_Value) path' val
                _ -> []
            Def_Name ->
              case node of
                NDef name _ _ ->
                  searchTreeBackward v (Path.snoc context Def_Name) path' name
                _ -> []
            Def_Args ->
              case node of
                NDef name args _ ->
                  searchTreeBackward v (Path.snoc context Def_Args) path' args <>
                  [ SearchEntry (Path.snoc context Def_Name) name ]
                _ -> []
            Def_Body ->
              case node of
                NDef name args body ->
                  searchTreeBackward v (Path.snoc context Def_Body) path' body <>
                  [ SearchEntry (Path.snoc context Def_Args) args
                  , SearchEntry (Path.snoc context Def_Name) name
                  ]
                _ -> []
            BinOp_Left ->
              case node of
                NBinOp _ left _ ->
                  searchTreeBackward v (Path.snoc context BinOp_Left) path' left
                _ -> []
            BinOp_Right ->
              case node of
                NBinOp _ left right->
                  searchTreeBackward v (Path.snoc context BinOp_Right) path' right <>
                  [ SearchEntry (Path.snoc context BinOp_Left) left ]
                _ -> []
            UnOp_Value ->
              case node of
                NUnOp _ val ->
                  searchTreeBackward v (Path.snoc context UnOp_Value) path' val
                _ -> []
            Call_Function ->
              case node of
                NCall func _ ->
                  searchTreeBackward v (Path.snoc context Call_Function) path' func
                _ -> []
            Call_Args ->
              case node of
                NCall func args->
                  searchTreeBackward v (Path.snoc context Call_Args) path' args <>
                  [ SearchEntry (Path.snoc context Call_Function) func ]
                _ -> []
            List_Exprs ->
              case node of
                NList exprs ->
                  searchTreeBackward v (Path.snoc context List_Exprs) path' exprs
                _ -> []
            Exprs_Index ix ->
              searchTreeBackwardList _NExprs _Exprs_Index ix v context path' node
            Block_Index ix ->
              case node of
                NBlock sts ->
                  let
                    (prefix, suffix) = splitAt ix . zip [0..] $ NonEmpty.toList sts
                  in
                    case suffix of
                      (_, st) : _ ->
                        searchTreeBackward v (Path.snoc context $ Block_Index ix) path' st <>
                        fmap (\(ix', st') -> SearchEntry (Path.snoc context $ Block_Index ix') st') prefix
                      [] -> []
            Args_Index ix ->
              searchTreeBackwardList _NArgs _Args_Index ix v context path' node
            Params_Index ix ->
              searchTreeBackwardList _NParams _Params_Index ix v context path' node

searchTreeBackwardList ::
  forall a x y z.
  Prism' (Node x) [Hash y] ->
  Prism' (Level x y) Int ->
  Int ->
  Versioned a ->
  Path a x ->
  Path y z ->
  Node x ->
  [SearchEntry a]
searchTreeBackwardList _Ctor _Level ix v context path node =
  case node ^? _Ctor of
    Just xs ->
      let
        (prefix, suffix) = splitAt ix $ zip [0..] xs
      in
        case suffix of
          (_, x) : _ ->
            searchTreeBackward v (Path.snoc context $ review _Level ix) path x <>
            fmap (\(ix', x') -> SearchEntry (Path.snoc context $ review _Level ix') x') prefix
          [] -> []
    Nothing -> []

prevHole :: forall a b. Versioned a -> Path a b -> Maybe (Focus a)
prevHole v focusPath = do
  let Identity (h, _) = runVersionedT v Versioned.getRoot
  let tree = searchTreeBackward v Nil focusPath h
  getAlt $ foldMap (\(SearchEntry sPath sH) -> Alt $ findPrevHole v sPath sH) tree
