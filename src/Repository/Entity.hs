-- | The Neo4j entity model.
module Repository.Entity where

import           Data.Text                      ( Text )

newtype ArtistSpotifyId = ArtistSpotifyId { unArtistSpotifyId :: Text } deriving Show
newtype AlbumSpotifyId = AlbumSpotifyId { unAlbumSpotifyId :: Text } deriving Show

newtype ArtistName = ArtistName { unArtistName :: Text } deriving Show
newtype AlbumName = AlbumName { unAlbumName :: Text } deriving Show
newtype SongName = SongName { unSongName :: Text } deriving Show

data Artist = Artist
  { artistSpotifyId :: ArtistSpotifyId
  , artistName :: Text
  } deriving Show

data Album = Album
  { albumSpotifyId :: AlbumSpotifyId
  , albumName :: Text
  , albumReleasedYear :: Int
  , albumTotalLength :: Int
  } deriving Show

data Song = Song
  { songNo :: Int
  , songTitle :: Text
  , songDuration :: Int
  } deriving Show
