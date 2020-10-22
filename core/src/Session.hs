{-# language BangPatterns #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances, UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Session
  ( MonadSession(..)
  , Session
  , newSession
  , getFocus
  , down
  , downGreatest
  , up
  , addChild
  , numChildren
  , SessionTree(..)
  , toTree
  )
where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Function (on)
import qualified Data.List as List

class Monad m => MonadSession a m | m -> a where
  undo :: m (Maybe a)
  default undo :: (m ~ t n, MonadTrans t, MonadSession a n) => m (Maybe a)
  undo = lift undo

  redo :: m (Maybe a)
  default redo :: (m ~ t n, MonadTrans t, MonadSession a n) => m (Maybe a)
  redo = lift redo

  getSession :: m (Session a)
  default getSession :: (m ~ t n, MonadTrans t, MonadSession a n) => m (Session a)
  getSession = lift getSession

instance MonadSession a m => MonadSession a (MaybeT m)

data Entry a
  = RootEntry
  { leftChildren :: [(a, SessionTree a)]
  , rightChildren :: [(a, SessionTree a)]
  }
  | Entry
  { label :: a
  , leftChildren :: [(a, SessionTree a)]
  , rightChildren :: [(a, SessionTree a)]
  } deriving Show

data Session a
  = Top (SessionTree a)
  | Session
  { history :: [Entry a]
  , focus :: a
  , current :: SessionTree a
  } deriving Show

newSession :: Session a
newSession = Top (Node [])

newtype SessionTree a
  = Node [(a, SessionTree a)]
  deriving Show

getFocus :: Session a -> Maybe a
getFocus sess =
  case sess of
    Top _ -> Nothing
    Session _ a _ -> Just a

down :: Int -> Session a -> Maybe (Session a)
down ix sess =
  case sess of
    Top (Node children) ->
      if 0 <= ix && ix < length children
      then
        let
          (prefix, rest) = List.splitAt ix children
        in
          case rest of
            [] -> error "impossible"
            (lbl, tree) : suffix ->
              Just $
              Session [RootEntry prefix suffix] lbl tree
      else Nothing
    Session hist foc (Node children) ->
      if 0 <= ix && ix < length children
      then
        let
          (prefix, rest) = List.splitAt ix children
        in
          case rest of
            [] -> error "impossible"
            (lbl, tree) : suffix ->
              Just $
              Session (Entry foc prefix suffix : hist) lbl tree
      else
        Nothing

data DownGreatestState a = Begin | Greatest Int a | Ambiguous

downGreatest :: (a -> a -> Ordering) -> Session a -> Maybe (Session a)
downGreatest comp sess =
  case sess of
    Top (Node children) -> do
      ix <- indexOfMax (comp `on` fst) children
      let (prefix, rest) = List.splitAt ix children
      case rest of
        [] -> error "impossible"
        (lbl, tree) : suffix ->
          Just $
          Session [RootEntry prefix suffix] lbl tree
    Session hist foc (Node children) -> do
      ix <- indexOfMax (comp `on` fst) children
      let (prefix, rest) = List.splitAt ix children
      case rest of
        [] -> error "impossible"
        (lbl, tree) : suffix ->
          Just $
          Session (Entry foc prefix suffix : hist) lbl tree
  where
    indexOfMax comp' xs =
      let
        (_, res) =
          foldl
            (\(!ix', !res') el ->
              ( ix' + 1
              , case res' of
                  Begin -> Greatest ix' el
                  Greatest ix g ->
                    case comp' g el of
                      LT -> Greatest ix' el
                      EQ -> Ambiguous
                      GT -> Greatest ix g
                  Ambiguous -> Ambiguous
              )
            )
            (0, Begin)
            xs
      in
        case res of
          Greatest ix _ -> Just ix
          _ -> Nothing

up :: Session a -> Maybe (Session a)
up sess =
  case sess of
    Top{} -> Nothing
    Session hist foc tree ->
      case hist of
        [] -> Nothing
        entry : hist' ->
          case entry of
            Entry prev lefts rights ->
              Just $
              Session hist' prev (Node $ lefts ++ (foc, tree) : rights)
            RootEntry lefts rights ->
              Just $
              Top (Node $ lefts ++ (foc, tree) : rights)

addChild :: a -> Session a -> (Int, Session a)
addChild a sess =
  case sess of
    Top (Node children) ->
      ( length children
      , Top . Node $ children ++ [(a, Node [])]
      )
    Session hist foc (Node children) ->
      ( length children
      , Session hist foc . Node $ children ++ [(a, Node [])]
      )

numChildren :: Session a -> Int
numChildren sess =
  case sess of
    Top (Node children) -> length children
    Session _ _ (Node children) -> length children

toTree :: Session a -> SessionTree a
toTree s =
  case go s of
    Top tree -> tree
    Session _ cur tree -> Node [(cur, tree)]
  where
    go sess = maybe sess go $ up sess
