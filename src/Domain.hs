{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain where

import           Data.Aeson
import           Data.Text.Lazy
import           Data.Time
import           GHC.Generics            (Generic (..))

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
        , avatarURL  :: Maybe Text
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
