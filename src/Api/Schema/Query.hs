{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

-- | The GraphQL schema for queries
module Api.Schema.Query where

import           Api.Args.Album                 ( AlbumArgs )
import qualified Api.Args.Album                as AlbumArgs
import           Api.Args.Artist                ( ArtistArgs )
import qualified Api.Args.Artist               as Args
import           Api.Dependencies               ( Deps )
import qualified Api.Dependencies              as D
import           Api.Domain.AlbumQL             ( AlbumQL
                                                , toAlbumQL
                                                )
import           Api.Domain.ArtistQL            ( ArtistQL
                                                , toArtistQL
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Morpheus.Types            ( IORes
                                                , resolver
                                                )
import           GHC.Generics                   ( Generic )
import           Repository.Album
import           Repository.Artist
import           Repository.Entity              ( ArtistName(..) )

data Query = Query
  { artist :: ArtistArgs -> IORes ArtistQL
  , albumsByArtist :: AlbumArgs -> IORes [AlbumQL]
  } deriving Generic

resolveArtist :: ArtistRepository IO -> ArtistArgs -> IORes ArtistQL
resolveArtist repo args = resolver result where
  result = findArtist repo (ArtistName $ Args.name args) <&> \case
    Just a  -> Right $ toArtistQL a
    Nothing -> Left "No hits"

resolveAlbumsByArtist :: AlbumRepository IO -> AlbumArgs -> IORes [AlbumQL]
resolveAlbumsByArtist repo args = resolver result where
  result = findAlbumsByArtist repo (ArtistName $ AlbumArgs.name args) <&> \case
    [] -> Left "No hits"
    xs -> Right $ toAlbumQL <$> xs

resolveQuery :: Deps -> Query
resolveQuery deps = Query
  { artist         = resolveArtist (D.artistRepository deps)
  , albumsByArtist = resolveAlbumsByArtist (D.albumRepository deps)
  }
