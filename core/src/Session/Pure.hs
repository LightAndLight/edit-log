{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
module Session.Pure
  ( SessionT
  , runSessionT
  )
where

import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Log (Time, Entry(..), MonadLog)
import Path (SomePath(..))
import Session (MonadSession(..), Session)
import qualified Session
import Store (MonadStore)
import Versioned (MonadVersioned(..), replaceH)
import Versioned.Pure (Versioned, VersionedT, runVersionedT)

newtype SessionT a m b
  = SessionT
  { unSessionT :: StateT (Session (Time, Entry a, SomePath a)) (VersionedT a m) b }
  deriving (Functor, Applicative, Monad, MonadLog a, MonadStore)

runSessionT ::
  Monad m =>
  Versioned a ->
  Session (Time, Entry a, SomePath a) ->
  SessionT a m b ->
  m (b, Versioned a, Session (Time, Entry a, SomePath a))
runSessionT v s m = do
  ((res, s'), v') <- runVersionedT v $ runStateT (unSessionT m) s
  pure (res, v', s')

trackVersioned ::
  Monad m =>
  MaybeT (VersionedT a m) (Time, Entry a, SomePath a) ->
  MaybeT (SessionT a m) (Time, Entry a)
trackVersioned m = do
  result@(t, e, _) <- MaybeT . SessionT . lift . runMaybeT $ m
  sess <- getSession
  let (ix, sess') = Session.addChild result sess
  case Session.down ix sess' of
    Nothing -> error "impossible"
    Just sess'' -> lift . SessionT $ put sess''
  pure (t, e)

instance Monad m => MonadVersioned a (SessionT a m) where
  replace p a =
    runMaybeT . trackVersioned $ do
      (t, e) <- MaybeT $ replace p a
      pure (t, e, SomePath p)

  replaceH p h =
    runMaybeT . trackVersioned $ do
      (t, e) <- MaybeT $ replaceH p h
      pure (t, e, SomePath p)

  insert p h = do
    runMaybeT . trackVersioned $ do
      (t, e) <- MaybeT $ insert p h
      pure (t, e, SomePath p)

  insertH p h =
    runMaybeT . trackVersioned $ do
      (t, e) <- MaybeT $ insertH p h
      pure (t, e, SomePath p)
  delete p ix =
    runMaybeT . trackVersioned $ do
      (t, e) <- MaybeT $ delete p ix
      pure (t, e, SomePath p)

  snapshot = SessionT $ lift snapshot

  getRoot = SessionT $ lift getRoot

instance Monad m => MonadSession (Time, Entry a, SomePath a) (SessionT a m) where
  undo = do
    sess <- getSession
    case Session.up sess of
      Nothing -> pure Nothing
      Just sess' -> do
        case Session.getFocus sess of
          Nothing -> error "impossible"
          Just result@(_, entry, _) -> do
            SessionT $ put sess'
            case entry of
              Replace path old _ ->
                SessionT $ do
                  _ <- lift $ replaceH path old
                  pure ()
              Insert path ix _ -> error "TODO: implement undo for Insert" path ix
              Delete path ix old ->
                SessionT $ do
                  _ <- lift $ insertH path (ix, old)
                  pure ()
            pure $ Just result

  redo = do
    sess <- getSession
    case Session.downGreatest (\(t, _, _) (t', _, _) -> compare t t') sess of
      Nothing -> pure Nothing
      Just sess' -> do
        case Session.getFocus sess' of
          Nothing -> error "impossible"
          Just result@(_, entry, _) -> do
            SessionT $ put sess'
            case entry of
              Replace path _ new ->
                SessionT $ do
                  _ <- lift $ replaceH path new
                  pure ()
              Insert path ix new ->
                SessionT $ do
                  _ <- lift $ insertH path (ix, new)
                  pure ()
              Delete path ix _ -> error "TODO implement redo for delete" path ix
            pure $ Just result

  getSession = SessionT get
