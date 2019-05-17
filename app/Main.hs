{-# LANGUAGE OverloadedStrings #-}

module Main where

import Db
import Routes

import Web.Scotty
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Database.MySQL.Simple
import Data.Pool(Pool, createPool, withResource)

-- Parse file "application.conf" and get the DB connection info
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password

main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf

    case dbConf of
      Nothing -> putStrLn "No database configuration found, terminating..."
      Just conf -> do
          pool <- createPool (newConn conf) close 1 64 10
          scotty 3000 $ routes pool
