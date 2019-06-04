{-# LANGUAGE OverloadedStrings #-}

module Entity.User where

import           Db

import           Database.MySQL.Simple (Connection, Only (Only))

import qualified Data.Text.Lazy        as TL

import           Data.Pool             (Pool)
import           Data.Time.Clock       (UTCTime)
import           Data.Tuple.Curry      (uncurryN)
import           Domain                (User (User))
import           Safe                  (headMay)

getUser :: Pool Connection -> TL.Text -> IO (Either TL.Text User)
getUser pool id = do
    userRes <-
        fetch
            pool
            (Only id)
            "SELECT id, login, avatarURL, name \
        \FROM user WHERE id=?" :: IO [(Integer, TL.Text, Maybe TL.Text, Maybe TL.Text)]
    case headMay userRes of
        Nothing -> return $ Left "No user with such id found"
        (Just firstUser) ->
            let user = uncurryN User firstUser
             in return $ Right user
