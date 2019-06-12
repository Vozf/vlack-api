{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           Db
import           Socket.App

import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import           Data.Pool                      (Pool, createPool, withResource)
import           Database.MySQL.Simple
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets             as WS
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
    let settings = Warp.setPort 3000 Warp.defaultSettings
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    case dbConf of
        Nothing -> putStrLn "No database configuration found, terminating..."
        Just conf -> do
            pool <- createPool (newConn conf) close 1 64 10
            app <- application pool
            Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions wsapp app
