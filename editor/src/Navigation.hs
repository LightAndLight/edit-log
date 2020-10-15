{-# language GADTs #-}
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
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Alt(..))
import qualified Data.List.NonEmpty as NonEmpty

import Hash (Hash)
import Node (Node(..))
import Path (Path(..), Level(..))
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
        NDef name _ body ->
          go (Path.snoc path Def_Name) name <|>
          go (Path.snoc path Def_Body) body
        NBool{} -> Nothing
        NInt{} -> Nothing
        NEIdent{} -> Nothing
        NBinOp _ left right ->
          go (Path.snoc path BinOp_Left) left <|>
          go (Path.snoc path BinOp_Right) right
        NUnOp _ val ->
          go (Path.snoc path UnOp_Value) val
        NBlock sts ->
          getAlt $
          foldMap
            (\(ix, st) -> Alt $ go (Path.snoc path $ Block_Index ix) st)
            (zip [0..] $ NonEmpty.toList sts)
        NIdent{} -> Nothing
        NSHole -> Just $ Focus path
        NEHole -> Just $ Focus path
        NIHole -> Just $ Focus path

nextHole :: forall a b. Versioned a -> Path a b -> Maybe (Focus a)
nextHole v focusPath = do
  let Identity (h, _) = runVersionedT v Versioned.getRoot
  let tree = searchTree Nil focusPath h
  getAlt $ foldMap (\(SearchEntry sPath sH) -> Alt $ findNextHole v sPath sH) tree
  where
    searchTree :: forall x y. Path a x -> Path x y -> Hash x -> [SearchEntry a]
    searchTree context path h =
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
                      searchTree (Path.snoc context For_Ident) path' ident <>
                      [ SearchEntry (Path.snoc context For_Expr) expr
                      , SearchEntry (Path.snoc context For_Block) body
                      ]
                    _ -> []
                For_Expr ->
                  case node of
                    NFor _ expr body ->
                      searchTree (Path.snoc context For_Expr) path' expr <>
                      [ SearchEntry (Path.snoc context For_Block) body ]
                    _ -> []
                For_Block ->
                  case node of
                    NFor _ _ body ->
                      searchTree (Path.snoc context For_Block) path' body
                    _ -> []
                IfThen_Cond ->
                  case node of
                    NIfThen cond then_ ->
                      searchTree (Path.snoc context IfThen_Cond) path' cond <>
                      [ SearchEntry (Path.snoc context IfThen_Then) then_ ]
                    _ -> []
                IfThen_Then ->
                  case node of
                    NIfThen _ then_ ->
                      searchTree (Path.snoc context IfThen_Then) path' then_
                    _ -> []
                IfThenElse_Cond ->
                  case node of
                    NIfThenElse cond then_ else_->
                      searchTree (Path.snoc context IfThenElse_Cond) path' cond <>
                      [ SearchEntry (Path.snoc context IfThenElse_Then) then_
                      , SearchEntry (Path.snoc context IfThenElse_Else) else_
                      ]
                    _ -> []
                IfThenElse_Then ->
                  case node of
                    NIfThenElse _ then_ else_->
                      searchTree (Path.snoc context IfThenElse_Then) path' then_ <>
                      [ SearchEntry (Path.snoc context IfThenElse_Else) else_ ]
                    _ -> []
                IfThenElse_Else ->
                  case node of
                    NIfThenElse _ _ else_->
                      searchTree (Path.snoc context IfThenElse_Else) path' else_
                    _ -> []
                Print_Value ->
                  case node of
                    NPrint val ->
                      searchTree (Path.snoc context Print_Value) path' val
                    _ -> []
                Def_Name ->
                  case node of
                    NDef name _ body ->
                      searchTree (Path.snoc context Def_Name) path' name <>
                      [ SearchEntry (Path.snoc context Def_Body) body ]
                    _ -> []
                Def_Body ->
                  case node of
                    NDef _ _ body ->
                      searchTree (Path.snoc context Def_Body) path' body
                    _ -> []
                BinOp_Left ->
                  case node of
                    NBinOp _ left right->
                      searchTree (Path.snoc context BinOp_Left) path' left <>
                      [ SearchEntry (Path.snoc context BinOp_Right) right ]
                    _ -> []
                BinOp_Right ->
                  case node of
                    NBinOp _ _ right->
                      searchTree (Path.snoc context BinOp_Right) path' right
                    _ -> []
                UnOp_Value ->
                  case node of
                    NUnOp _ val ->
                      searchTree (Path.snoc context UnOp_Value) path' val
                    _ -> []
                Block_Index ix ->
                  case node of
                    NBlock sts ->
                      let
                        (_, suffix) = splitAt ix . zip [0..] $ NonEmpty.toList sts
                      in
                        case suffix of
                          (_, st) : rest ->
                            searchTree (Path.snoc context $ Block_Index ix) path' st <>
                            fmap (\(ix', st') -> SearchEntry (Path.snoc context $ Block_Index ix') st') rest
                          [] -> []

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
        NDef name _ body ->
          go (Path.snoc path Def_Name) name <|>
          go (Path.snoc path Def_Body) body
        NBool{} -> Nothing
        NInt{} -> Nothing
        NEIdent{} -> Nothing
        NBinOp _ left right ->
          go (Path.snoc path BinOp_Right) right <|>
          go (Path.snoc path BinOp_Left) left
        NUnOp _ val ->
          go (Path.snoc path UnOp_Value) val
        NBlock sts ->
          getAlt $
          foldMap
            (\(ix, st) -> Alt $ go (Path.snoc path $ Block_Index ix) st)
            (reverse . zip [0..] $ NonEmpty.toList sts)
        NIdent{} -> Nothing
        NSHole -> Just $ Focus path
        NEHole -> Just $ Focus path
        NIHole -> Just $ Focus path

prevHole :: forall a b. Versioned a -> Path a b -> Maybe (Focus a)
prevHole v focusPath = do
  let Identity (h, _) = runVersionedT v Versioned.getRoot
  let tree = searchTree Nil focusPath h
  getAlt $ foldMap (\(SearchEntry sPath sH) -> Alt $ findPrevHole v sPath sH) tree
  where
    searchTree :: forall x y. Path a x -> Path x y -> Hash x -> [SearchEntry a]
    searchTree context path h =
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
                      searchTree (Path.snoc context For_Ident) path' ident
                    _ -> []
                For_Expr ->
                  case node of
                    NFor ident expr _ ->
                      searchTree (Path.snoc context For_Expr) path' expr <>
                      [ SearchEntry (Path.snoc context For_Ident) ident ]
                    _ -> []
                For_Block ->
                  case node of
                    NFor ident expr body ->
                      searchTree (Path.snoc context For_Block) path' body <>
                      [ SearchEntry (Path.snoc context For_Expr) expr
                      , SearchEntry (Path.snoc context For_Ident) ident
                      ]
                    _ -> []
                IfThen_Cond ->
                  case node of
                    NIfThen cond _ ->
                      searchTree (Path.snoc context IfThen_Cond) path' cond
                    _ -> []
                IfThen_Then ->
                  case node of
                    NIfThen cond then_ ->
                      searchTree (Path.snoc context IfThen_Then) path' then_ <>
                      [ SearchEntry (Path.snoc context IfThen_Cond) cond ]
                    _ -> []
                IfThenElse_Cond ->
                  case node of
                    NIfThenElse cond _ _ ->
                      searchTree (Path.snoc context IfThenElse_Cond) path' cond
                    _ -> []
                IfThenElse_Then ->
                  case node of
                    NIfThenElse cond then_ _ ->
                      searchTree (Path.snoc context IfThenElse_Then) path' then_ <>
                      [ SearchEntry (Path.snoc context IfThenElse_Cond) cond ]
                    _ -> []
                IfThenElse_Else ->
                  case node of
                    NIfThenElse cond then_ else_->
                      searchTree (Path.snoc context IfThenElse_Else) path' else_ <>
                      [ SearchEntry (Path.snoc context IfThenElse_Then) then_
                      , SearchEntry (Path.snoc context IfThenElse_Cond) cond
                      ]
                    _ -> []
                Print_Value ->
                  case node of
                    NPrint val ->
                      searchTree (Path.snoc context Print_Value) path' val
                    _ -> []
                Def_Name ->
                  case node of
                    NDef name _ _ ->
                      searchTree (Path.snoc context Def_Name) path' name
                    _ -> []
                Def_Body ->
                  case node of
                    NDef name _ body ->
                      searchTree (Path.snoc context Def_Body) path' body <>
                      [ SearchEntry (Path.snoc context Def_Name) name ]
                    _ -> []
                BinOp_Left ->
                  case node of
                    NBinOp _ left _ ->
                      searchTree (Path.snoc context BinOp_Left) path' left
                    _ -> []
                BinOp_Right ->
                  case node of
                    NBinOp _ left right->
                      searchTree (Path.snoc context BinOp_Right) path' right <>
                      [ SearchEntry (Path.snoc context BinOp_Left) left ]
                    _ -> []
                UnOp_Value ->
                  case node of
                    NUnOp _ val ->
                      searchTree (Path.snoc context UnOp_Value) path' val
                    _ -> []
                Block_Index ix ->
                  case node of
                    NBlock sts ->
                      let
                        (prefix, suffix) = splitAt ix . zip [0..] $ NonEmpty.toList sts
                      in
                        case suffix of
                          (_, st) : _ ->
                            searchTree (Path.snoc context $ Block_Index ix) path' st <>
                            fmap (\(ix', st') -> SearchEntry (Path.snoc context $ Block_Index ix') st') prefix
                          [] -> []
