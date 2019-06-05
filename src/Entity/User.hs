{-# LANGUAGE OverloadedStrings #-}

module Entity.User where

import           Db

import           Database.MySQL.Simple (Connection, Only (Only))

import qualified Data.Text.Lazy        as TL

import           Data.Hash.MD5         (Str (Str), md5s)
import           Data.Pool             (Pool)
import           Data.Text.Lazy        (unpack)
import           Data.Time.Clock       (UTCTime)
import           Data.Tuple.Curry      (uncurryN)
import           Domain                (RegisterCredentials (RegisterCredentials),
                                        User (User))
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

findUserByLogin :: Pool Connection -> String -> IO (Maybe (Integer, String))
findUserByLogin pool login = do
    res <-
        fetch pool (Only login) "SELECT id, password FROM user WHERE login=?" :: IO [( Integer
                                                                                     , String)]
    return $ idAndPassword res
  where
    idAndPassword [(userId, pwd)] = Just (userId, pwd)
    idAndPassword _ = Nothing

registerUser :: Pool Connection -> Maybe RegisterCredentials -> IO (Either TL.Text ())
registerUser pool Nothing = return $ Left "Empty User"
registerUser pool (Just (RegisterCredentials login password avatarURL name)) = do
    execSqlT
        pool
        (login, md5s $ Str $ unpack password, avatarURL, name)
        "INSERT INTO user(login, password, avatarURL, name) VALUES(?,?,?,?)"
    return $ Right ()
