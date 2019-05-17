{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           Db

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Hash.MD5
import           Data.Pool             (Pool, createPool, withResource)
import           Data.String
import           Database.MySQL.Simple

-- Encodes provided password with md5 and then compares it with the hash
-- stored in the DB
verifyCredentials :: Pool Connection -> B.ByteString -> B.ByteString -> IO Bool
verifyCredentials pool user password = do
    pwd <- findUserByLogin pool (BC.unpack user)
    return $ comparePasswords pwd (BC.unpack password)
  where
    comparePasswords Nothing _         = False
    comparePasswords (Just p) password = p == (md5s $ Str password)
