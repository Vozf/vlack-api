{-# LANGUAGE OverloadedStrings #-}

module Domain where

import           Control.Applicative
import           Data.Aeson
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding

data Article =
    Article Integer Text Text -- id title bodyText
    deriving (Show)

instance FromJSON Article where
    parseJSON (Object v) =
        Article <$> v .:? "id" .!= 0 <*> -- the field "id" is optional
        v .: "title" <*>
        v .: "bodyText"

instance ToJSON Article where
    toJSON (Article id title bodyText) =
        object ["id" .= id, "title" .= title, "bodyText" .= bodyText]
