module Api.Dependencies where

import           Config                         ( SpotifyConfig )
import           Repository.Album               ( AlbumRepository )
import           Repository.Artist              ( ArtistRepository )

data Deps = Deps
  { spotifyConfig :: SpotifyConfig
  , artistRepository :: ArtistRepository IO
  , albumRepository :: AlbumRepository IO
  }
