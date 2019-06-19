{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           Db
import           Socket.App

import           Control.Applicative            ((<|>))
import           Control.Concurrent.Chan        (newChan)
import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import           Data.Maybe                     (fromJust, fromMaybe)
import           Data.Pool                      (Pool, createPool, withResource)
import           Data.Text                      (pack)
import           Database.MySQL.Simple
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import qualified Network.WebSockets             as WS
import           System.Environment             (lookupEnv)
import           Text.Read                      (readMaybe)
import           Web.Scotty

-- Parse file "application.conf" and get the DB connection info
-- If there are env variables - use them(Heroku deploy)
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
    let readApplicationConf key = C.lookup conf key :: IO (Maybe String)
        readEnvOrConf key = do
            envVal <- lookupEnv key
            confVal <- readApplicationConf (pack key)
            return $ envVal <|> confVal
    name <- readEnvOrConf "database.name"
    host <- readEnvOrConf "database.host"
    user <- readEnvOrConf "database.user"
    password <- readEnvOrConf "database.password"
    return $ DbConfig <$> name <*> host <*> user <*> password

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
            let port = fromMaybe 3000 (portEnv >>= readMaybe) :: Int
                settings = Warp.setPort port Warp.defaultSettings
                wsApp = wsapplication pool chan
                app = WaiWs.websocketsOr WS.defaultConnectionOptions wsApp httpApp
            putStrLn $ "Starting server on port " ++ show port
            Warp.runSettings settings app
