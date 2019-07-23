{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Http.Client.Params where

import           Data.Aeson
import           Data.Text
import           GHC.Generics                   ( Generic )

newtype AccessToken = AccessToken { unAccessToken :: Text } deriving (Generic, Show)
newtype ArtistId = ArtistId { unArtistId :: Text } deriving Show

instance FromJSON AccessToken where
  parseJSON = withObject "f" $ \v -> AccessToken
    <$> v .: "access_token"
