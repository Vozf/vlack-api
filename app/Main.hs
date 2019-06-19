{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           Db
import           Socket.App

import           Control.Concurrent.Chan        (newChan)
import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import           Data.Maybe                     (fromMaybe)
import           Data.Pool                      (Pool, createPool, withResource)
import           Database.MySQL.Simple
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets             as WS
import           System.Environment             (lookupEnv)
import           Text.Read                      (readMaybe)
import           Web.Scotty

-- Parse file "application.conf" and get the DB connection info
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
    name <- C.lookup conf "database.name" :: IO (Maybe String)
    user <- C.lookup conf "database.user" :: IO (Maybe String)
    password <- C.lookup conf "database.password" :: IO (Maybe String)
    return $ DbConfig <$> name <*> user <*> password

main :: IO ()
main = do
    portEnv <- lookupEnv "PORT"
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    case dbConf of
        Nothing -> putStrLn "No database configuration found, terminating..."
        Just conf -> do
            pool <- createPool (newConn conf) close 1 64 10
            chan <- newChan
            httpApp <- httpApplication pool
            let port = maybe 3000 read (portEnv >>= readMaybe) :: Int
                settings = Warp.setPort port Warp.defaultSettings
                wsApp = wsapplication pool chan
                app = WaiWs.websocketsOr WS.defaultConnectionOptions wsApp httpApp
            putStrLn $ "Starting server on port " ++ show port
            Warp.runSettings settings app
