{-# LANGUAGE OverloadedStrings #-}

module App
    ( httpApplication
    ) where

import           Auth
import           CorsMiddleware
import           Db
import           Domain
import           Entity.Chat
import           Entity.Message
import           Entity.User
import           TokenAuth
import           Views

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Pool                            (Pool, createPool,
                                                       withResource)
import           Data.Set                             (Set, fromList, member)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Database.MySQL.Simple
import           Network.HTTP.Types.Status            (status201, status400)
import           Network.Wai
import           Network.Wai.Middleware.HttpAuth
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Network.Wai.Middleware.Static
import           Web.Scotty
import           Web.Scotty.Internal.Types            (ActionT)

httpApplication :: Pool Connection -> IO Application
httpApplication pool = scottyApp $ do
    middleware allowCors
    middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
    middleware logStdout -- log all requests; for production use logStdout
    middleware $
        tokenAuth
            (verifyToken superSecretJWT pool) -- check if the user is authenticated for protected resources
            "Haskell Blog Realm" {TokenAuth.authIsProtected = protectedResources} -- function which restricts access to some routes only for authenticated users
    get "/chats" $ do
        chats <- liftIO $ listChats pool
        chatWithLastMessageList chats
    get "/chats/:id" $ do
        id <- param "id" :: ActionM TL.Text
        eitherRes <- liftIO $ findChat pool id
        case eitherRes of
            Left e -> do
                status status400
                text e
            Right chatWithMessages -> viewChatWithMessages chatWithMessages
    post "/chats/:chatId" $ do
        chatId <- param "chatId" :: ActionM TL.Text
        userId <- getUserIdFromToken
        message <- decodeBody :: ActionM (Maybe NewMessageBody)
        eitherRes <- liftIO $ insertMessage pool chatId userId message
        case eitherRes of
            Left e -> do
                status status400
                text e
            Right _ -> status status201
    get "/users/:id" $ do
        id <- param "id" :: ActionM TL.Text
        eitherRes <- liftIO $ getUser pool id
        case eitherRes of
            Left e -> do
                status status400
                text e
            Right user -> Web.Scotty.json user
    post "/login" $ do
        loginCredentials <- decodeBody :: ActionM (Maybe LoginCredentials)
        tokenEither <- liftIO $ createTokenFromCredentials pool superSecretJWT loginCredentials
        case tokenEither of
            Left st -> status status400 >> Web.Scotty.text st
            Right token -> Web.Scotty.json token
    post "/register" $ do
        registerCredentials <- decodeBody :: ActionM (Maybe RegisterCredentials)
        regRes <- liftIO $ registerUser pool registerCredentials
        case regRes of
            Left e -> status status400 >> text e
            Right user -> status status201

-- The function knows which resources are available only for the
-- authenticated users
protectedResources :: Request -> IO Bool
protectedResources request = do
    let path = pathInfo request
    return $ protect path
  where
    protect (p:_) = not $ member p unProtectedSet
    protect _     = True

unProtectedSet :: Set T.Text
unProtectedSet = fromList ["login", "register"]

decodeBody :: FromJSON a => ActionM (Maybe a)
decodeBody = decode <$> body
