{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Result (..), Value, decode, fromJSON,
                                         toJSON)
import qualified Data.ByteString        as B
import           Data.Char              (isSpace)
import           Data.Hash.MD5          (Str (..), md5s)
import           Data.Map               (fromList, (!?))
import           Data.Pool              (Pool, createPool, withResource)
import           Data.String
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Text.Lazy         as TL
import           Database.MySQL.Simple
import           Domain                 (LoginCredentials (LoginCredentials),
                                         User (User), password)
import           Entity.User            (findUserByLogin, getUser)
import qualified Web.JWT                as JWT
import           Web.Scotty             (ActionM, header)

verifyToken :: T.Text -> Pool Connection -> B.ByteString -> IO Bool
verifyToken jwtSecret pool token =
    case JWT.decodeAndVerifySignature (JWT.hmacSecret jwtSecret) (decodeUtf8 token) of
        Nothing -> return False
        Just _  -> return True

createTokenFromCredentials ::
       Pool Connection -> T.Text -> Maybe LoginCredentials -> IO (Either TL.Text TL.Text)
createTokenFromCredentials pool jwtSecret Nothing = return $ Left "No credentials"
createTokenFromCredentials pool jwtSecret (Just (LoginCredentials login password)) = do
    pwd <- findUserByLogin pool (TL.unpack login)
    let comparePasswords Nothing _ = Left "No such user"
        comparePasswords (Just (userId, p)) password =
            if p == (md5s $ Str password)
                then Right userId
                else Left "Incorrect password"
        userIdEither = comparePasswords pwd (TL.unpack password)
    userEither <-
        case userIdEither of
            Left st      -> return $ Left st
            Right userId -> getUser pool (TL.pack $ show userId)
    return (jsonToToken jwtSecret . toJSON <$> userEither)

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
