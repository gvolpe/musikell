{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE BlockArguments, LambdaCase, RecordWildCards #-}

-- | The GraphQL schema for queries
module Api.Schema.Query where

import           Api.Args.Album                 ( AlbumArgs )
import qualified Api.Args.Album                as AlbumArgs
import           Api.Args.Artist                ( ArtistArgs )
import qualified Api.Args.Artist               as Args
import           Api.Dependencies               ( Deps(..) )
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
resolveArtist ArtistRepository {..} args = resolver result where
  result = findArtist (ArtistName $ Args.name args) <&> \case
    Just a  -> Right $ toArtistQL a
    Nothing -> Left "No hits"

resolveAlbumsByArtist :: AlbumRepository IO -> AlbumArgs -> IORes [AlbumQL]
resolveAlbumsByArtist AlbumRepository {..} args = resolver result where
  result = findAlbumsByArtist (ArtistName $ AlbumArgs.name args) <&> \case
    [] -> Left "No hits"
    xs -> Right $ toAlbumQL <$> xs

resolveQuery :: Deps -> Query
resolveQuery Deps {..} = Query
  { artist         = resolveArtist artistRepository
  , albumsByArtist = resolveAlbumsByArtist albumRepository
  }
