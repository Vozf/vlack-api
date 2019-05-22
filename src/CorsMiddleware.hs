{-# LANGUAGE OverloadedStrings #-}

module CorsMiddleware (allowCors) where

import Network.Wai.Middleware.Cors
import Network.Wai (Middleware)

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }
