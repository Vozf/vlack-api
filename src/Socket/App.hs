{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Socket.App where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever)
import           Data.Aeson                 (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Map                   (Map)
import           Data.Text
import           GHC.Generics               (Generic)
import qualified Network.WebSockets         as WS
import           Socket.Domain

wsapp :: WS.ServerApp
wsapp pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    (msg :: ByteString) <- WS.receiveData conn
    let socketMessage = decode msg :: Maybe SocketMsgWithoutPayload
        messageType = msgType <$> socketMessage
    putStrLn $ Data.ByteString.Lazy.Char8.unpack $ encode socketMessage
    case messageType of
        (Just msgType) -> WS.sendTextData conn $ ("initial> " :: ByteString) <> encode messageType
        _ -> WS.sendTextData conn $ ("fail> " :: ByteString) <> msg
