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

import Log (Time, Entry(..))
import Session (MonadSession(..), Session)
import qualified Session
import Versioned (MonadVersioned(..), replaceH)
import Versioned.Pure (Versioned, VersionedT, runVersionedT)

newtype SessionT a m b
  = SessionT
  { unSessionT :: StateT (Session (Time, Entry a)) (VersionedT a m) b }
  deriving (Functor, Applicative, Monad)

runSessionT ::
  Monad m =>
  Versioned a ->
  Session (Time, Entry a) ->
  SessionT a m b -> m (b, Versioned a, Session (Time, Entry a))
runSessionT v s m = do
  ((res, s'), v') <- runVersionedT v $ runStateT (unSessionT m) s
  pure (res, v', s')

instance Monad m => MonadVersioned a (SessionT a m) where
  replace p a = do
    m_result <- SessionT . lift $ replace p a
    case m_result of
      Nothing -> pure Nothing
      Just result -> do
        sess <- getSession
        let (ix, sess') = Session.addChild result sess
        case Session.down ix sess' of
          Nothing -> error "impossible"
          Just sess'' -> SessionT $ put sess''
        pure $ Just result

  replaceH p h = do
    m_result <- SessionT . lift $ replaceH p h
    case m_result of
      Nothing -> pure Nothing
      Just result -> do
        sess <- getSession
        let (ix, sess') = Session.addChild result sess
        case Session.down ix sess' of
          Nothing -> error "impossible"
          Just sess'' -> SessionT $ put sess''
        pure $ Just result

  snapshot = SessionT $ lift snapshot

instance Monad m => MonadSession (Time, Entry a) (SessionT a m) where
  undo = do
    sess <- getSession
    case Session.up sess of
      Nothing -> pure False
      Just sess' -> do
        case Session.getFocus sess of
          Nothing -> error "impossible"
          Just (_, Replace path old _) -> do
            SessionT $ do
              put sess'
              _ <- lift $ replaceH path old
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
              Just (_, Replace path _ new) ->
                SessionT $ do
                  put sess'
                  _ <- lift $ replaceH path new
                  pure True
      else pure False

  getSession = SessionT get
