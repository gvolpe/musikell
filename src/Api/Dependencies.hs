module Api.Dependencies where

import           Repository.Album               ( AlbumRepository )
import           Repository.Artist              ( ArtistRepository )

data Deps = Deps
  { artistRepository :: ArtistRepository IO
  , albumRepository :: AlbumRepository IO
  }
