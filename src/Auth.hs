{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           Db

import           Data.Aeson            (Result (..), Value, decode, fromJSON)
import qualified Data.ByteString       as B
import           Data.Char             (isSpace)
import           Data.Map              (fromList, (!?))
import           Data.Pool             (Pool, createPool, withResource)
import           Data.String
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)
import qualified Data.Text.Lazy        as TL
import           Database.MySQL.Simple
import           Domain                (User (User))
import qualified Web.JWT               as JWT
import           Web.Scotty            (ActionM, header)

verifyCredentials :: T.Text -> Pool Connection -> B.ByteString -> IO Bool
verifyCredentials jwtSecret pool token =
    case JWT.decodeAndVerifySignature (JWT.hmacSecret jwtSecret) (decodeUtf8 token) of
        Nothing -> return False
        Just _  -> return True

getUserFromToken :: ActionM (Maybe User)
getUserFromToken = do
    tokenMay <- getToken
    let token = tokenMay >>= tokenToJson
        user =
            case token of
                Nothing -> Nothing
                Just token ->
                    case fromJSON token of
                        Error _   -> Nothing
                        Success a -> Just a :: Maybe User
    return user

getToken :: ActionM (Maybe TL.Text)
getToken = do
    str <- header "Authorization"
    return $ str >>= extractToken

extractToken :: TL.Text -> Maybe TL.Text
extractToken auth
    | TL.toLower (fromString x) == "bearer" = Just $ fromString $ dropWhile isSpace y
    | otherwise = Nothing
  where
    (x, y) = break isSpace (TL.unpack auth)

jsonToToken :: T.Text -> Value -> TL.Text
jsonToToken jwtSecret user =
    let cs = mempty {JWT.unregisteredClaims = JWT.ClaimsMap $ fromList [(jwtKey, user)]}
        key = JWT.hmacSecret jwtSecret
     in TL.fromStrict $ JWT.encodeSigned key mempty cs

tokenToJson :: TL.Text -> Maybe Value
tokenToJson token = do
    jwt <- JWT.decode (TL.toStrict token)
    JWT.unClaimsMap (JWT.unregisteredClaims (JWT.claims jwt)) !? jwtKey

superSecretJWT :: T.Text
superSecretJWT = "secret"

jwtKey :: T.Text
jwtKey = "jwt"
