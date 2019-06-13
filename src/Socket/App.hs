{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Socket.App where

import           Control.Concurrent         (forkIO, killThread, threadDelay)
import           Control.Concurrent.Chan    (Chan, dupChan, readChan, writeChan)
import           Control.Exception          (SomeException (..), handle)
import           Control.Monad              (forever)
import           Control.Monad.Fix          (fix)
import           Data.Aeson                 (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 as BS (pack, unpack)
import           Data.Map                   (Map)
import           Data.Text
import           GHC.Generics               (Generic)
import qualified Network.WebSockets         as WS
import           Socket.Controller
import           Socket.Domain

wsapplication :: Chan ChanMsg -> WS.ServerApp
wsapplication chan pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    runConn conn chan

runConn :: WS.Connection -> Chan ChanMsg -> IO ()
runConn conn chan = do
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
            let messageType = msgType <$> (decode msg :: Maybe SocketMsgWithoutPayload)
            case messageType of
                (Just NewMessageType) ->
                    newMessage chan (decode msg :: Maybe (SocketMsg NewMessage))
                _ -> WS.sendTextData conn $ ("fail> " :: ByteString) <> msg
            loop
    killThread reader -- kill after the loop ends
