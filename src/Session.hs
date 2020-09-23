{-# language ViewPatterns #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
module Session
  ( MonadSession(..)
  , Session
  , newSession
  , getFocus
  , down
  , up
  , addChild
  , numChildren
  , SessionTree(..)
  , toTree
  )
where

import qualified Data.List as List

class Monad m => MonadSession a m | m -> a where
  undo :: m Bool
  redo :: m Bool

  getSession :: m (Session a)

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
