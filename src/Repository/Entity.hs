-- | The Neo4j entity model.
module Repository.Entity where

import           Control.Monad.Catch            ( Exception )
import           Data.Text

newtype ArtistName = ArtistName { unArtistName :: Text } deriving Show
newtype AlbumName = AlbumName { unAlbumName :: Text } deriving Show
newtype SongName = SongName { unSongName :: Text } deriving Show

newtype SpotifyId = SpotifyId { unSpotifyId :: Text } deriving Show

data MissingSpotifyId = MissingSpotifyId deriving Show
instance Exception MissingSpotifyId

data Artist = Artist
  { artistSpotifyId :: Text
  , artistName :: Text
  } deriving Show

data Album = Album
  { albumSpotifyId :: Text
  , albumName :: Text
  , albumReleasedYear :: Int
  , albumTotalLength :: Int
  } deriving Show

data Song = Song
  { songNo :: Int
  , songTitle :: Text
  , songDuration :: Int
  } deriving Show
