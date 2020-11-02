{-# language LambdaCase #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
module List.Dynamic (listDyn) where

import Control.Monad.Fix (MonadFix)
import Reflex hiding (list)
import qualified Reflex.Dom as Dom

listDyn ::
  forall t m a.
  (Adjustable t m, MonadHold t m, MonadFix m) =>
  Dynamic t [a] ->
  m (Dynamic t [Dynamic t a])
listDyn dList = do
  rec
    dListD <-
      Dom.widgetHold
        (inner dList)
        (attachWithMaybe
          (\now next ->
             case now of
               [] ->
                 case next of
                   [] ->
                     Nothing
                   _ ->
                     Just $ inner dList
               _:_ ->
                 case next of
                   _:_ ->
                     Nothing
                   _ ->
                     Just $ inner dList
          )
          (current dListD)
          (updated dList)
        )
  pure dListD
  where
    inner :: Dynamic t [a] -> m [Dynamic t a]
    inner dList' = do
      list <- sample $ current dList'
      go (updated dList') list

    go :: Event t [a] -> [a] -> m [Dynamic t a]
    go eList' list =
      case list of
        [] ->
          pure []
        x : xs ->
          (:) <$>
          holdDyn
            x
            (fmapMaybe
               (\case; x' : _ -> Just x'; _ -> Nothing)
               eList'
            ) <*>
          (do
             let
               eList'' =
                 (fmapMaybe
                   (\case; _ : xs' -> Just xs'; _ -> Nothing)
                   eList'
                 )
             go eList'' xs
          )
