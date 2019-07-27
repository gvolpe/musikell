module Api.Dependencies where

import           Http.Client.Spotify            ( SpotifyClient )
import           Repository.Album               ( AlbumRepository )
import           Repository.Artist              ( ArtistRepository )

data Deps = Deps
  { artistRepository :: ArtistRepository IO
  , albumRepository :: AlbumRepository IO
  , spotifyClient :: SpotifyClient IO
  }
