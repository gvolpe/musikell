-- | The Domain model.
module Domain where

import           Data.Text

newtype ArtistId = ArtistId { unArtistId :: Int } deriving Show
newtype AlbumId = AlbumId { unAlbumId :: Int } deriving Show

data Artist = Artist
  { artistName :: Text
  , artistOrigin :: Text
  } deriving Show

data Album = Album
  { albumName :: Text
  , albumReleasedYear :: Int
  , albumTotalLength :: Int
  } deriving Show

data Song = Song
  { songNo :: Int
  , songTitle :: Text
  , songDuration :: Int
  } deriving Show
