{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity.Chat where

import           Db

import           Database.MySQL.Simple

import qualified Data.Text.Lazy        as TL

import           Data.Pool             (Pool)
import           Data.Time             (UTCTime)
import           Data.Tuple.Curry
import           Domain                (Chat (Chat), ChatWithLastMessage (ChatWithLastMessage),
                                        ChatWithMessages (ChatWithMessages),
                                        Message (Message))
import           Safe                  (headMay)

listChats :: Pool Connection -> IO [ChatWithLastMessage]
listChats pool = do
    chatsAndMessagesRes <-
        fetchSimple
            pool
            "SELECT chat.*, message.*, user.name, user.avatarURL  FROM chat INNER JOIN message\
              	\ ON message.id = (SELECT id FROM message WHERE chatId = chat.id order by createdAt DESC limit 1)\
                \ join user on message.userId = user.id\
              \ ORDER BY chat.createdAt;" :: IO [( Integer
                                                 , TL.Text
                                                 , Integer
                                                 , UTCTime
                                                 , Integer
                                                 , TL.Text
                                                 , Integer
                                                 , Integer
                                                 , UTCTime
                                                 , UTCTime
                                                 , TL.Text
                                                 , Maybe TL.Text)]
    let mapChatAndMessage (chat, message) =
            ChatWithLastMessage (uncurryN Chat chat) (uncurryN Message message)
        splitChatAndMessage (c1, c2, c3, c4, m1, m2, m3, m4, m5, m6, m7, m8) =
            ((c1, c2, c3, c4), (m1, m2, m3, m4, m5, m6, m7, m8))
        getChatWithMessage res = mapChatAndMessage . splitChatAndMessage <$> res
     in return $ getChatWithMessage chatsAndMessagesRes

findChat :: Pool Connection -> TL.Text -> IO (Either TL.Text ChatWithMessages)
findChat pool id = do
    chatRes <-
        fetch pool (Only id) "SELECT * FROM chat WHERE id=?" :: IO [( Integer
                                                                    , TL.Text
                                                                    , Integer
                                                                    , UTCTime)]
    messagesRes <-
        fetch
            pool
            (Only id)
            "SELECT message.*, user.name, user.avatarURL FROM message\
         \ join user on message.userId = user.id WHERE chatId=? order by createdAt" :: IO [( Integer
                                                                                           , TL.Text
                                                                                           , Integer
                                                                                           , Integer
                                                                                           , UTCTime
                                                                                           , UTCTime
                                                                                           , TL.Text
                                                                                           , Maybe TL.Text)]
    case headMay chatRes of
        Nothing -> return $ Left "No chat with such id found"
        (Just chat) ->
            let firstChat = uncurryN Chat chat
                messages = uncurryN Message <$> messagesRes
             in return $ Right $ ChatWithMessages firstChat messages
