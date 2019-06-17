{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module Socket.Domain
    ( NewMessageRequest(..)
    , NewMessageResponse(..)
    , SocketMsgWithoutPayload(..)
    , SocketMsgRequestType(..)
    , prepare
    , SocketMsgRequest(payload)
    , ChanMsg
    ) where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Domain
import           GHC.Generics         (Generic)

type ChanMsg = ByteString

data SocketMsgRequestType
    = NewMessageRequestType
    | OldMessageRequestType
    deriving (Show, Generic)

instance FromJSON SocketMsgRequestType

class (FromJSON a) =>
      SocketMessageRequest a
    where
    getType :: a -> SocketMsgRequestType

data SocketMsgRequest a =
    SocketMsgRequest
        { msgType :: SocketMsgRequestType
        , payload :: a
        , token   :: String
        }
    deriving (Show, Generic)

instance (FromJSON a) => FromJSON (SocketMsgRequest a)

data SocketMsgWithoutPayload =
    SocketMsgWithoutPayload
        { msgType :: SocketMsgRequestType
        , token   :: String
        }
    deriving (Show, Generic)

instance FromJSON SocketMsgWithoutPayload

-- socket message for response
data SocketMsgResponseType
    = NewMessageResponseType
    | OldMessageResponseType
    deriving (Show, Generic)

instance ToJSON SocketMsgResponseType

instance FromJSON SocketMsgResponseType

class (ToJSON a) =>
      SocketMessageResponse a
    where
    getResponseType :: a -> SocketMsgResponseType
    prepare :: a -> SocketMsgResponse a
    prepare a = SocketMsgResponse (getResponseType a) a

data SocketMsgResponse a =
    SocketMsgResponse
        { msgType :: SocketMsgResponseType
        , payload :: a
        }
    deriving (Show, Generic)

instance (ToJSON a) => ToJSON (SocketMsgResponse a)

-- request socket messages below
data NewMessageRequest =
    NewMessageRequest
        { chatId :: Integer
        , value  :: String
        }
    deriving (Show, Generic)

instance FromJSON NewMessageRequest

instance SocketMessageRequest NewMessageRequest where
    getType = const NewMessageRequestType

--response socket messages below
type NewMessageResponse = Message

instance SocketMessageResponse NewMessageResponse where
    getResponseType = const NewMessageResponseType
