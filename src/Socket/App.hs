{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Socket.App where

import           Auth
import           Control.Concurrent         (forkIO, killThread, threadDelay)
import           Control.Concurrent.Chan    (Chan, dupChan, readChan, writeChan)
import           Control.Exception          (SomeException (..), handle)
import           Control.Monad              (forever)
import           Control.Monad.Fix          (fix)
import           Data.Aeson                 (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 as BS (pack, unpack)
import           Data.Map                   (Map)
import           Data.Pool                  (Pool)
import           Data.Text
import qualified Data.Text.Lazy             as TL
import           Database.MySQL.Simple      (Connection)
import           GHC.Generics               (Generic)
import qualified Network.WebSockets         as WS
import           Socket.Controller

import           Socket.Domain

wsapplication :: Pool Connection -> Chan ChanMsg -> WS.ServerApp
wsapplication pool chan pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    runConn pool chan conn

runConn :: Pool Connection -> Chan ChanMsg -> WS.Connection -> IO ()
runConn pool chan conn = do
    let broadcast = writeChan chan
    commLine <- dupChan chan
    -- fork off a thread for reading from the duplicated channel
    reader <-
        forkIO $
        fix $ \loop -> do
            line <- readChan commLine
            WS.sendTextData conn line
            loop
    handle (\(SomeException _) -> return ()) $
        fix $ \loop -> do
            (msg :: ByteString) <- WS.receiveData conn
            let message = decode msg :: Maybe SocketMsgWithoutPayload
                messageType = msgType <$> message
            case messageType of
                (Just NewMessageRequestType) ->
                    newMessage
                        pool
                        chan
                        userMay
                        (decode msg :: Maybe (SocketMsgRequest NewMessageRequest))
                    where tok = token <$> message
                          userMay = TL.pack <$> tok >>= getUserFromToken
                _ -> WS.sendTextData conn $ ("fail> " :: ByteString) <> msg
            loop
    killThread reader -- kill after the loop ends
