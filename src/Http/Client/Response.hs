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
  { albumItems :: [AlbumItem]
  } deriving (Generic, Show)

data ArtistItem = ArtistItem
  { artistId :: Text
  , artistName :: Text
  } deriving (Generic, Show)

newtype ArtistObject = ArtistObject
  { artistItems :: [ArtistItem]
  } deriving (Generic, Show)

-- TODO: See if this can be simplified
newtype ArtistResponse = ArtistResponse
  { artistObject :: ArtistObject
  } deriving (Generic, Show)

instance FromJSON AlbumItem where
  parseJSON = withObject "item" $ \v -> AlbumItem
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "release_date"

instance FromJSON AlbumResponse where
  parseJSON = withObject "items" $ \v -> AlbumResponse
    <$> v .: "items"

instance FromJSON ArtistItem where
  parseJSON = withObject "item" $ \v -> ArtistItem
    <$> v .: "id"
    <*> v .: "name"

instance FromJSON ArtistObject where
  parseJSON = withObject "items" $ \v -> ArtistObject
    <$> v .: "items"

instance FromJSON ArtistResponse where
  parseJSON = withObject "artists" $ \v -> ArtistResponse
    <$> v .: "artists"
