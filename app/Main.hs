{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad
import           Control.Monad.Except
import qualified System.Environment   as Env

import           InfoBot

main :: IO ()
main =
  void $ do
    maybeToken <- Env.lookupEnv "API_TOKEN"
    case maybeToken of
      Nothing -> putStrLn "$API_TOKEN not set"
      Just apiToken -> do
        result <- runExceptT (setup apiToken)
        case result of
          Right (inbox, outbox) -> do
            jazzChan inbox outbox
            lunchChan inbox outbox
            eventChan inbox
            errChan inbox
            logChan inbox
            sinkChan inbox
          Left err -> do
            putStrLn "connection failed"
            print err
