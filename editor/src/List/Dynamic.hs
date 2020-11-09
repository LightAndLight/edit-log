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
  [a] ->
  Dynamic t [a] ->
  m ([(a, Dynamic t a)], Dynamic t [(a, Dynamic t a)])
listDyn initialList dList = do
  rec
    initialListD <- inner initialList dList
    dListD <-
      Dom.widgetHold
        (pure initialListD)
        (attachWithMaybe
          (\now next ->
             case now of
               [] ->
                 case next of
                   [] ->
                     Nothing
                   _ ->
                     Just $ inner next dList
               _:_ ->
                 case next of
                   _:_ ->
                     Nothing
                   _ ->
                     Just $ inner next dList
          )
          (current dListD)
          (updated dList)
        )
  pure (initialListD, dListD)
  where
    inner :: [a] -> Dynamic t [a] -> m [(a, Dynamic t a)]
    inner initialList' dList' = do
      go (updated dList') initialList'

    go :: Event t [a] -> [a] -> m [(a, Dynamic t a)]
    go eList' list =
      case list of
        [] ->
          pure []
        x : xs ->
          (:) <$>
          ((,) x <$>
           holdDyn
             x
             (fmapMaybe
                (\case; x' : _ -> Just x'; _ -> Nothing)
                eList'
             )
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
