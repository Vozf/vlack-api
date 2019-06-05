{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Domain where

import           Control.Applicative
import           Data.Aeson
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           Data.Time
import           GHC.Generics            (Generic (..))

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
    deriving (Show, Generic)

instance ToJSON Chat

newtype NewMessageBody = NewMessageBody{value :: Text}
                           deriving (Show, Generic)

instance FromJSON NewMessageBody

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
    deriving (Show, Generic)

instance ToJSON Message

data ChatWithLastMessage =
    ChatWithLastMessage
        { chat        :: Chat
        , lastMessage :: Message
        }
    deriving (Show, Generic)

instance ToJSON ChatWithLastMessage

data ChatWithMessages =
    ChatWithMessages
        { chat     :: Chat
        , messages :: [Message]
        }
    deriving (Show, Generic)

instance ToJSON ChatWithMessages

data User =
    User
        { id        :: Integer
        , login     :: Text
        , avatarURL :: Maybe Text
        , name      :: Maybe Text
        }
    deriving (Show, Generic)

instance ToJSON User

instance FromJSON User

data LoginCredentials =
    LoginCredentials
        { login    :: Text
        , password :: Text
        }
    deriving (Show, Generic)

instance FromJSON LoginCredentials

data RegisterCredentials =
    RegisterCredentials
        { login     :: Text
        , password  :: Text
        , avatarURL :: Maybe Text
        , name      :: Text
        }
    deriving (Show, Generic)

instance FromJSON RegisterCredentials
