-- | The Neo4j entity model.
module Repository.Entity where

import           Data.Text

newtype ArtistId = ArtistId { unArtistId :: Int } deriving Show
newtype AlbumId = AlbumId { unAlbumId :: Int } deriving Show

newtype ArtistName = ArtistName { unArtistName :: Text } deriving Show
newtype AlbumName = AlbumName { unAlbumName :: Text } deriving Show
newtype SongName = SongName { unSongName :: Text } deriving Show

data Artist = Artist
  { artistName :: Text
  , artistSpotifyId :: Text
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
