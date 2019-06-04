{-# LANGUAGE OverloadedStrings #-}

module Entity.Message where

import           Db

import           Database.MySQL.Simple (Connection)

import qualified Data.Text.Lazy        as TL

import           Data.Pool             (Pool)
import           Domain                (NewMessageBody (NewMessageBody))

insertMessage :: Pool Connection -> TL.Text -> Maybe NewMessageBody -> IO (Either TL.Text ())
insertMessage pool _ Nothing = return $ Left "Message can't be parsed"
insertMessage pool chatId (Just (NewMessageBody value userId)) = do
    execSqlT pool (value, userId, chatId) "INSERT INTO message(value, userId, chatId) VALUES(?,?,?)"
    return $ Right ()
