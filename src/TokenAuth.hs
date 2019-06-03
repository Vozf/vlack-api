{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

--override funcs from httpAuth to work with token
module TokenAuth
    ( tokenAuth
    , authIsProtected
    ) where

import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as BC

import           Data.String
import           Data.Text.Lazy
import           Network.HTTP.Types.Header       (hAuthorization, hContentType)
import           Network.HTTP.Types.Status       (status401)
import           Network.Wai                     (Application, Middleware,
                                                  responseLBS)
import           Network.Wai.Internal            (Request, requestHeaders)
import           Network.Wai.Middleware.HttpAuth (CheckCreds, extractBearerAuth)
import           Web.JWT                         as JWT

type CheckToken = BC.ByteString -> IO Bool

tokenAuth :: CheckToken -> AuthSettings -> Middleware
tokenAuth checkToken AuthSettings {..} app req sendResponse = do
    isProtected <- authIsProtected req
    allowed <-
        if isProtected
            then check
            else return True
    if allowed
        then app req sendResponse
        else authOnNoAuth authRealm req sendResponse
  where
    check =
        case lookup hAuthorization (requestHeaders req) >>= extractBearerAuth of
            Nothing -> return False
            Just token -> checkToken token

data AuthSettings =
    AuthSettings
        { authRealm       :: !BC.ByteString
        , authOnNoAuth    :: !(BC.ByteString -> Application)
        , authIsProtected :: !(Request -> IO Bool)
        }

instance IsString AuthSettings where
    fromString s =
        AuthSettings
            { authRealm = fromString s
            , authOnNoAuth =
                  \realm _req f ->
                      f $
                      responseLBS
                          status401
                          [ (hContentType, "text/plain")
                          , ("WWW-Authenticate", B.concat ["Basic realm=\"", realm, "\""])
                          ]
                          "Basic authentication is required"
            , authIsProtected = const $ return True
            }
