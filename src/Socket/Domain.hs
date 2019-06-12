{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module Socket.Domain
    ( NewMessage(..)
    , SocketMsgWithoutPayload(..)
    , SocketMsgType(..)
    , prepare
    , SocketMsg(payload)
    ) where

import           GHC.Generics (Generic)

import           Data.Aeson

data SocketMsgType
    = NewMessageType
    | OldMessageType
    deriving (Show, Generic)

instance ToJSON SocketMsgType

instance FromJSON SocketMsgType

class (FromJSON a, ToJSON a) =>
      SocketMessage a
    where
    getType :: a -> SocketMsgType
    prepare :: a -> SocketMsg a
    prepare a = SocketMsg (getType a) a

data SocketMsg a =
    SocketMsg
        { msgType :: SocketMsgType
        , payload :: a
        }
    deriving (Show, Generic)

instance (ToJSON a) => ToJSON (SocketMsg a)

instance (FromJSON a) => FromJSON (SocketMsg a)

newtype SocketMsgWithoutPayload =
    SocketMsgWithoutPayload
        { msgType :: SocketMsgType
        }
    deriving (Show, Generic)

instance ToJSON SocketMsgWithoutPayload

instance FromJSON SocketMsgWithoutPayload

-- socket messages below
data NewMessage =
    NewMessage
        { chatId :: Integer
        , value  :: String
        }
    deriving (Show, Generic)

instance ToJSON NewMessage

instance FromJSON NewMessage

instance SocketMessage NewMessage where
    getType _ = NewMessageType
