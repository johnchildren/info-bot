{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module InfoBot.Modules where

import           BasePrelude        hiding (putStrLn)
import           Control.Lens
import qualified Data.Map.Strict           as Map
import           Data.Text          as Text
import           Data.Text.IO

import           InfoBot.Connection
import           InfoBot.Types

logChan :: Inbox -> IO ()
logChan inbox = withInbox inbox putStrLn

eventChan :: Inbox -> IO ()
eventChan inbox =
  withInbox inbox $ \event_ -> putStrLn $ "event occured: " <> (event_ ^. kind)

errChan :: Inbox -> IO ()
errChan inbox =
  withInbox inbox $ \error_ ->
    putStrLn $ "error occured: " <> (error_ ^. errbody)

sinkChan :: Inbox -> IO ()
sinkChan inbox = (void . forever) $ readChan inbox

jazzChan :: Inbox -> Outbox -> IO ()
jazzChan inbox outbox = do
  countM <- newMVar (0 :: Int)
  withInbox inbox $ \line_ ->
    when
      (Text.isInfixOf ":raised_hands" (line_ ^. message))
      (do let victory =
                writeOutbox outbox (Line (line_ ^. channel) "jazz" "JAZZ HANDS")
          let update = (`mod` 3) . (+ 1) &&& id
          count <- modifyMVar countM (return . update)
          when (count == 2) victory)

data LunchAction = Order Text | Ordered | NoAction

lunchAction :: Text -> LunchAction
lunchAction msg | "order" == lowerHead msg = Order $ onlyTail msg
lunchAction msg | "ordered" == lowerHead msg = Ordered
lunchAction msg = NoAction

lowerHead :: Text -> Text
lowerHead = Text.toLower . BasePrelude.head . Text.words

onlyTail :: Text -> Text
onlyTail = Text.unwords . BasePrelude.tail . Text.words

lunchChan :: Inbox -> Outbox -> IO ()
lunchChan inbox outbox = do
    ordersM <- newMVar Map.empty
    withInbox inbox $ \line_ ->
      case lunchAction (line_ ^. message) of
        Order orderText -> do
            putStrLn $ "got order" <> orderText
            let update = Map.insert (line_ ^. username) orderText
            modifyMVar_ ordersM (return . update)
        Ordered -> do
            putStrLn "dumping orders"
            orders <- readMVar ordersM
            writeOutbox outbox (Line (line_ ^. channel) "orders" (Text.pack $ show orders))
        NoAction -> return ()
