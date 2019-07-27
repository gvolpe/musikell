{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Http.Client.Params where

import           Data.Aeson
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype AccessToken = AccessToken { unAccessToken :: Text } deriving (Generic, Show)
newtype ArtistId = ArtistId { unArtistId :: Text } deriving Show
newtype ArtistName = ArtistName { unArtistName :: Text } deriving Show

newtype AlbumId = AlbumId { unAlbumId :: Text } deriving (Eq, Ord, Show)

instance FromJSON AccessToken where
  parseJSON = withObject "f" $ \v -> AccessToken
    <$> v .: "access_token"
