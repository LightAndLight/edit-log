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

import Log (Time, Entry(..), MonadLog)
import Session (MonadSession(..), Session)
import qualified Session
import Store (MonadStore)
import Versioned (MonadVersioned(..), replaceH)
import Versioned.Pure (Versioned, VersionedT, runVersionedT)

newtype SessionT a m b
  = SessionT
  { unSessionT :: StateT (Session (Time, Entry a)) (VersionedT a m) b }
  deriving (Functor, Applicative, Monad, MonadLog a, MonadStore)

runSessionT ::
  Monad m =>
  Versioned a ->
  Session (Time, Entry a) ->
  SessionT a m b ->
  m (b, Versioned a, Session (Time, Entry a))
runSessionT v s m = do
  ((res, s'), v') <- runVersionedT v $ runStateT (unSessionT m) s
  pure (res, v', s')

trackVersioned ::
  Monad m =>
  VersionedT a m (Maybe (Time, Entry a)) ->
  SessionT a m (Maybe (Time, Entry a))
trackVersioned m = do
  m_result <- SessionT $ lift m
  case m_result of
    Nothing -> pure Nothing
    Just result -> do
      sess <- getSession
      let (ix, sess') = Session.addChild result sess
      case Session.down ix sess' of
        Nothing -> error "impossible"
        Just sess'' -> SessionT $ put sess''
      pure $ Just result

instance Monad m => MonadVersioned a (SessionT a m) where
  replace p a = trackVersioned $ replace p a
  replaceH p h = trackVersioned $ replaceH p h
  insert p h = trackVersioned $ insert p h
  insertH p h = trackVersioned $ insertH p h
  delete p ix = trackVersioned $ delete p ix

  snapshot = SessionT $ lift snapshot

  getRoot = SessionT $ lift getRoot

instance Monad m => MonadSession (Time, Entry a) (SessionT a m) where
  undo = do
    sess <- getSession
    case Session.up sess of
      Nothing -> pure False
      Just sess' -> do
        case Session.getFocus sess of
          Nothing -> error "impossible"
          Just (_, entry) -> do
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
            pure True

  redo = do
    sess <- getSession
    if Session.numChildren sess == 1
      then do
        case Session.down 0 sess of
          Nothing -> error "impossible"
          Just sess' -> do
            case Session.getFocus sess' of
              Nothing -> error "impossible"
              Just (_, entry) -> do
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
                pure True
      else pure False

  getSession = SessionT get
