{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module InfoBot.Connection where

import           BasePrelude             hiding (lazy)
import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Text.Strict.Lens
import           Network.Linklater
import           Network.Linklater.Types
import           Network.Socket
import           Network.WebSockets      (ClientApp, forkPingThread,
                                          receiveData, sendTextData)
import           URI.ByteString
import           Wuss

import           InfoBot.Types

class MonadCSP m where
  newChan_ :: m (Chan a)
  writeChan_ :: Chan a -> a -> m ()
  readChan_ :: Chan a -> m a
  dupChan_ :: Chan a -> m (Chan a)
  forkIO_ :: m () -> m ThreadId

instance MonadCSP IO where
  newChan_ = newChan
  writeChan_ = writeChan
  readChan_ = readChan
  dupChan_ = dupChan
  forkIO_ = forkIO

class MonadSecureClient m where
  runSecureClient_ :: HostName -> PortNumber -> String -> ClientApp a -> m a

instance MonadSecureClient IO where
  runSecureClient_ = runSecureClient

instance MonadCSP (ExceptT RequestError IO) where
  newChan_ = lift newChan_
  writeChan_ chan msg = lift $ writeChan_ chan msg
  readChan_ = lift . readChan_
  dupChan_ = lift . dupChan_
  forkIO_ = lift . forkIO_ . void . runExceptT

instance MonadSecureClient (ExceptT RequestError IO) where
  runSecureClient_ host port path app =
    lift $ runSecureClient_ host port path app

setup ::
     (MonadError RequestError m, MonadCSP m, MonadIO m, MonadSecureClient m)
  => String
  -> m (Inbox, Outbox)
setup apiToken = do
  outbox <- newChan_
  uri <- startRTM (APIToken (apiToken ^. packed))
  inbox <- serve uri outbox
  return (inbox, outbox)

serve :: (MonadCSP m, MonadSecureClient m, Monad m) => URI -> Outbox -> m Inbox
serve uri outbox =
  case ( uri ^? authorityL . _Just . authorityHostL . hostBSL . utf8 . unpacked
       , uri ^? pathL . utf8 . unpacked) of
    (Just host, Just path) -> do
      chan <- newChan_
      forkIO_ $ runSecureClient_ host 443 path (consumer chan)
      return chan
    _ -> error ("invalid url: " <> show uri)
  where
    consumer :: Inbox -> ClientApp ()
    consumer inbox conn = do
      forkPingThread conn 30
      forkIO (forever listener)
      forever worker
      where
        worker = do
          msg <- receiveData conn
          writeChan inbox msg
        listener = do
          message <- readChan outbox
          sendTextData conn message

withInbox :: (FromJSON a, MonadCSP m, Monad m) => Inbox -> (a -> m b) -> m ()
withInbox inbox cont = do
  chan <- dupChan_ inbox
  (void . forkIO_ . forever) $ do
    bytes <- readChan_ chan
    case eitherDecode (bytes ^. lazy) of
      Left _  -> return ()
      Right o -> void (cont o)

writeOutbox :: (ToJSON a, Show a, MonadCSP m) => Outbox -> a -> m ()
writeOutbox outbox msg =
  writeChan_ outbox $ encode msg ^. strict
