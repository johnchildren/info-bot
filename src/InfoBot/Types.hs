{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module InfoBot.Types where

import           BasePrelude
import           Control.Concurrent.Chan
import           Control.Lens            hiding ((.=))
import           Data.Aeson
import           Data.ByteString
import           Data.Text

type Bytes = ByteString

type Inbox = Chan Bytes

type Outbox = Chan Bytes

newtype Event = Event
  { _kind :: Text
  }

data RTMError = RTMError
  { _errcode :: Int
  , _errbody :: !Text
  } deriving (Eq, Ord, Show)

data Line = Line
  { _channel  :: !Text
  , _username :: !Text
  , _message  :: !Text
  } deriving (Eq, Ord, Show)

makeLenses ''Event

makeLenses ''RTMError

makeLenses ''Line

instance FromJSON Event where
  parseJSON (Object o) = Event <$> o .: "type"

instance FromJSON RTMError where
  parseJSON (Object o) = do
    msgtype <- o .: "type"
    guard ((msgtype :: Text) == "error")
    RTMError <$> ((o .: "error") >>= (.: "code")) <*>
      ((o .: "error") >>= (.: "msg"))

instance FromJSON Line where
  parseJSON (Object o) = do
    reply_to <- o .:? "reply_to"
    guard (not (isReply reply_to))
    Line <$> o .: "channel" <*> o .: "user" <*> o .: "text"
    where
      isReply :: Maybe Int -> Bool
      isReply = isJust

instance ToJSON Line where
  toJSON (Line channel username message) =
    object
      [ "type" .= ("message" :: Text)
      , "channel" .= channel
      , "username" .= username
      , "text" .= message
      ]
