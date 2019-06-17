{-# LANGUAGE OverloadedStrings #-}

module Entity.Message where

import           Db

import           Database.MySQL.Simple (Connection)

import qualified Data.Text.Lazy        as TL

import           Data.Pool             (Pool)
import           Data.Time             (UTCTime)
import           Domain                (NewMessageBody (NewMessageBody), Message(..))
import Safe (headMay)
import Data.Tuple.Curry (uncurryN)

insertMessage ::
       Pool Connection -> TL.Text -> Maybe Integer -> Maybe NewMessageBody -> IO (Either TL.Text Message)
insertMessage pool chatId (Just userId) (Just (NewMessageBody value)) = do
    execSqlT pool (value, userId, chatId) "INSERT INTO message(value, userId, chatId) VALUES(?,?,?)"
    messagesRes <-
        fetchSimple
            pool
            "SELECT message.*, user.name, user.avatarURL FROM message\
         \ join user on message.userId = user.id WHERE message.id=LAST_INSERT_ID()" :: IO [( Integer
                                                                                           , TL.Text
                                                                                           , Integer
                                                                                           , Integer
                                                                                           , UTCTime
                                                                                           , UTCTime
                                                                                           , TL.Text
                                                                                           , Maybe TL.Text)]
    case headMay messagesRes of
        Nothing -> return $ Left "Message wasn't created"
        (Just message) ->
            let firstMessage = uncurryN Message message
             in return $ Right firstMessage
insertMessage pool _ _ _ = return $ Left "Message can't be parsed"
