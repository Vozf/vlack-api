{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Domain where

import           Control.Applicative
import           Data.Aeson
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           Data.Time

data Article =
    Article Integer Text Text -- id title bodyText
    deriving (Show)

instance FromJSON Article where
    parseJSON (Object v) =
        Article <$> v .:? "id" .!= 0 <*> -- the field "id" is optional
        v .: "title" <*>
        v .: "bodyText"

instance ToJSON Article where
    toJSON (Article id title bodyText) =
        object ["id" .= id, "title" .= title, "bodyText" .= bodyText]

data Chat =
    Chat
        { id        :: Integer
        , title     :: Text
        , userId    :: Integer
        , createdAt :: UTCTime
        }

instance ToJSON Chat where
    toJSON (Chat id title userId createdAt) =
        object ["id" .= id, "title" .= title, "userId" .= userId, "createdAt" .= createdAt]

data Message =
    Message
        { id        :: Integer
        , value     :: Text
        , userId    :: Integer
        , chatId    :: Integer
        , createdAt :: UTCTime
        , updatedAt :: UTCTime
        }
    deriving (Show)

zeroTime :: UTCTime
zeroTime = UTCTime (fromGregorian 1 0 0) (secondsToDiffTime 0)

instance FromJSON Message where
    parseJSON (Object v) =
        Message <$> v .:? "id" .!= 0 <*> -- the field "id" is optional
        v .: "value" <*>
        v .: "userId" <*>
        v .:? "chatId" .!= 0 <*>
        v .:? "createdAt" .!= zeroTime <*>
        v .:? "updatedAt" .!= zeroTime

instance ToJSON Message where
    toJSON (Message id value userId chatId createdAt updatedAt) =
        object
            [ "id" .= id
            , "value" .= value
            , "userId" .= userId
            , "chatId" .= chatId
            , "createdAt" .= createdAt
            , "updatedAt" .= updatedAt
            ]

data ChatWithLastMessage =
    ChatWithLastMessage
        { chat        :: Chat
        , lastMessage :: Message
        }

instance ToJSON ChatWithLastMessage where
    toJSON (ChatWithLastMessage chat lastMessage) =
        object
            [ "chat" .= chat
            , "lastMessage" .= lastMessage
            ]

data ChatWithMessages =
    ChatWithMessages
        { chat     :: Chat
        , messages :: [Message]
        }

instance ToJSON ChatWithMessages where
    toJSON (ChatWithMessages chat messages) = object ["chat" .= chat, "messages" .= messages]
