{-# LANGUAGE OverloadedStrings #-}

module Routes
    ( routes
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

routes :: Pool Connection -> ScottyM ()
routes pool = do
    middleware allowCors
    middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
    middleware $ logStdout -- log all requests; for production use logStdout
    middleware $
        tokenAuth
            (verifyToken superSecretJWT pool) -- check if the user is authenticated for protected resources
            "Haskell Blog Realm" {TokenAuth.authIsProtected = protectedResources} -- function which restricts access to some routes only for authenticated users
                       -- LIST
    get "/articles" $ do
        articles <- liftIO $ listArticles pool -- get the ist of articles for DB
        articlesList articles -- show articles list
                       -- VIEW
    get "/articles/:id" $ do
        id <- param "id" :: ActionM TL.Text -- get the article id from the request
        maybeArticle <- liftIO $ findArticle pool id -- get the article from the DB
        viewArticle maybeArticle -- show the article if it was found
                       -- CREATE
    post "/admin/articles" $ do
        article <- getArticleParam -- read the request body, try to parse it into article
        insertArticle pool article -- insert the parsed article into the DB
        createdArticle article -- show info that the article was created
                       -- UPDATE
    put "/admin/articles" $ do
        article <- getArticleParam -- read the request body, try to parse it into article
        updateArticle pool article -- update parsed article in the DB
        updatedArticle article -- show info that the article was updated
                       -- DELETE
    delete "/admin/articles/:id" $ do
        id <- param "id" :: ActionM TL.Text -- get the article id
        deleteArticle pool id -- delete the article from the DB
        deletedArticle id -- show info that the article was deleted
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
            Left st     -> status status400 >> Web.Scotty.text st
            Right token -> Web.Scotty.json token
    post "/register" $ do
        registerCredentials <- decodeBody :: ActionM (Maybe RegisterCredentials)
        regRes <- liftIO $ registerUser pool registerCredentials
        case regRes of
            Left e     -> status status400 >> text e
            Right user -> status status201

-- The function knows which resources are available only for the
-- authenticated users
protectedResources :: Request -> IO Bool
protectedResources request = do
    let path = pathInfo request
    return $ protect path
  where
    protect (p:_) = not $ member p protectedSet
    protect _     = True

protectedSet :: Set T.Text
protectedSet = fromList ["login", "register"]

-- Parse the request body into the Article
getArticleParam :: ActionT TL.Text IO (Maybe Article)
getArticleParam = do
    b <- body
    return $ (decode b :: Maybe Article)
  where
    makeArticle s = ""

decodeBody :: FromJSON a => ActionM (Maybe a)
decodeBody = decode <$> body
