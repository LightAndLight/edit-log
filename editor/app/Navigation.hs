{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
module Navigation
  ( findNextHole
  , nextHole
  )
where

import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Alt(..))

import Hash (Hash)
import Node (Node(..))
import Path (Path(..), Level(..))
import qualified Path
import qualified Store
import qualified Versioned
import Versioned.Pure (Versioned, runVersionedT)

import Focus (Focus(..))

findNextHole :: Versioned a -> Path x y -> Hash y -> Maybe (Focus x)
findNextHole v = go
  where
    go :: Path b c -> Hash c -> Maybe (Focus b)
    go path h = do
      let Identity (mNode, _) = runVersionedT v $ Store.lookupNode h
      node <- mNode
      case node of
        NFor _ expr body ->
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
        NBool{} -> Nothing
        NInt{} -> Nothing
        NBinOp _ left right ->
          go (Path.snoc path BinOp_Left) left <|>
          go (Path.snoc path BinOp_Right) right
        NUnOp _ val ->
          go (Path.snoc path UnOp_Value) val
        NBlock sts ->
          getAlt $
          foldMap
            (\(ix, st) -> Alt $ go (Path.snoc path $ Block_Index ix) st)
            (zip [0..] sts)
        NSHole -> Just $ Focus path
        NEHole -> Just $ Focus path

data SearchEntry a where
  SearchEntry :: Path a x -> Hash x -> SearchEntry a
deriving instance Show (SearchEntry a)

nextHole :: forall a b. Versioned a -> Path a b -> Maybe (Focus a)
nextHole v focusPath = do
  let
    Identity ((h, mNode), _) = runVersionedT v $ do
      rooth <- Versioned.getRoot
      (,) rooth <$> Store.lookupNode h
  node <- mNode
  case node of
    NSHole -> Nothing
    NEHole -> Nothing
    _ -> do
      let tree = searchTree Nil focusPath h
      getAlt $ foldMap (\(SearchEntry sPath sH) -> Alt $ findNextHole v sPath sH) tree
  where
    searchTree :: forall x y. Path a x -> Path x y -> Hash x -> [SearchEntry a]
    searchTree context path h =
      case path of
        Nil -> [SearchEntry context h]
        Cons l path' -> do
          let Identity (mNode, _) = runVersionedT v $ Store.lookupNode h
          case mNode of
            Nothing -> []
            Just node ->
              case l of
                For_Ident ->
                  case node of
                    NFor ident _ _ -> error "TODO idents aren't hashed" ident
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
                        (_, suffix) = splitAt ix $ zip [0..] sts
                      in
                        case suffix of
                          (_, st) : rest ->
                            searchTree (Path.snoc context $ Block_Index ix) path' st <>
                            fmap (\(ix', st') -> SearchEntry (Path.snoc context $ Block_Index ix') st') rest
                          [] -> []
