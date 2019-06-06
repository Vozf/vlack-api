{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module SocketApp where

import Data.Text
import qualified Network.WebSockets as WS
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

wsapp :: WS.ServerApp
wsapp pending = do
    putStrLn "ws connected"
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    (msg :: Text) <- WS.receiveData conn
    WS.sendTextData conn $ ("initial> " :: Text) <> msg
    forever $ do
        WS.sendTextData conn ("loop data" :: Text)
        threadDelay $ 1 * 1000000
