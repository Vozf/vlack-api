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

data NewMessageBody =
    NewMessageBody
        { value  :: Text
        , userId :: Integer
        }

instance FromJSON NewMessageBody where
    parseJSON (Object v) = NewMessageBody <$> v .: "value" <*> v .: "userId"

data Message =
    Message
        { id         :: Integer
        , value      :: Text
        , userId     :: Integer
        , chatId     :: Integer
        , createdAt  :: UTCTime
        , updatedAt  :: UTCTime
        , authorName :: Text
        , avatarURL  :: Text
        }
    deriving (Show)

instance ToJSON Message where
    toJSON (Message id value userId chatId createdAt updatedAt authorName avatarURL) =
        object
            [ "id" .= id
            , "value" .= value
            , "userId" .= userId
            , "chatId" .= chatId
            , "createdAt" .= createdAt
            , "updatedAt" .= updatedAt
            , "authorName" .= authorName
            , "avatarURL" .= avatarURL
            ]

data ChatWithLastMessage =
    ChatWithLastMessage
        { chat        :: Chat
        , lastMessage :: Message
        }

instance ToJSON ChatWithLastMessage where
    toJSON (ChatWithLastMessage chat lastMessage) =
        object ["chat" .= chat, "lastMessage" .= lastMessage]

data ChatWithMessages =
    ChatWithMessages
        { chat     :: Chat
        , messages :: [Message]
        }

instance ToJSON ChatWithMessages where
    toJSON (ChatWithMessages chat messages) = object ["chat" .= chat, "messages" .= messages]

data User =
    User
        { id        :: Integer
        , login     :: Text
        , avatarURL :: Maybe Text
        , name      :: Maybe Text
        }

instance ToJSON User where
    toJSON (User id login avatarURL name) =
        object
            [ "id" .= id
            , "login" .= login
            , "avatarURL" .= avatarURL
            , "name" .= name
            ]

instance FromJSON User where
    parseJSON (Object v) =
        User <$> v .: "id" <*> -- the field "id" is optional
        v .: "login" <*>
        v .: "avatarURL" <*>
        v .: "name"
