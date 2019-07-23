{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Http.Client.Response where

import           Data.Aeson
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )

data AlbumItem = AlbumItem
  { albumId :: Text
  , albumName :: Text
  , albumReleaseDate :: Text
  } deriving (Generic, Show)

newtype AlbumResponse = AlbumResponse
  { items :: [AlbumItem]
  } deriving (Generic, Show)

instance FromJSON AlbumItem where
  parseJSON = withObject "item" $ \v -> AlbumItem
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "release_date"

instance FromJSON AlbumResponse
