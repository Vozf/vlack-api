{-# LANGUAGE OverloadedStrings #-}

module Entity.Message where

import           Db

import           Database.MySQL.Simple (Connection)

import qualified Data.Text.Lazy        as TL

import           Data.Pool             (Pool)
import           Domain                (NewMessageBody (NewMessageBody))

insertMessage ::
       Pool Connection -> TL.Text -> Maybe Integer -> Maybe NewMessageBody -> IO (Either TL.Text ())
insertMessage pool chatId (Just userId) (Just (NewMessageBody value)) = do
    execSqlT pool (value, userId, chatId) "INSERT INTO message(value, userId, chatId) VALUES(?,?,?)"
    return $ Right ()
insertMessage pool _ _ _ = return $ Left "Message can't be parsed"
