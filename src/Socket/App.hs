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
import           Socket.Domain

wsapplication :: Chan String -> WS.ServerApp
wsapplication chan pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    runConn conn chan

runConn :: WS.Connection -> Chan String -> IO ()
runConn conn chan = do
    let broadcast = writeChan chan
    WS.sendTextData conn ("Hi, what's your name?" :: ByteString)
    (name :: Text) <- WS.receiveData conn
    broadcast ("--> " ++ show name ++ " entered chat.")
    WS.sendTextData conn (BS.pack ("Welcome, " ++ show name ++ "!"))
    commLine <- dupChan chan
    -- fork off a thread for reading from the duplicated channel
    reader <-
        forkIO $
        fix $ \loop -> do
            line <- readChan commLine
            WS.sendTextData conn (BS.pack line)
            loop
    handle (\(SomeException _) -> return ()) $
        fix $ \loop -> do
            (line :: Text) <- WS.receiveData conn
            case line
             -- If an exception is caught, send a message and break the loop
                  of
                "quit" -> WS.sendTextData conn ("Bye" :: ByteString)
             -- else, continue looping.
                _      -> broadcast (show name ++ ": " ++ show line) >> loop
    killThread reader -- kill after the loop ends
    broadcast ("<-- " ++ show name ++ " left.") -- make a final broadcast
