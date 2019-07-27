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
import           Data.Morpheus.Types            ( ResM
                                                , gqlResolver
                                                )
import           GHC.Generics                   ( Generic )
import           Repository.Album
import           Repository.Artist
import           Repository.Entity              ( ArtistName(..) )

data Query = Query
  { artist :: ArtistArgs -> ResM ArtistQL
  , albumsByArtist :: AlbumArgs -> ResM [AlbumQL]
  } deriving Generic

resolveArtist :: ArtistRepository IO -> ArtistArgs -> ResM ArtistQL
resolveArtist repo args = gqlResolver result where
  result = findArtist repo (ArtistName $ Args.name args) <&> \case
    Just a  -> Right $ toArtistQL a
    Nothing -> Left "No hits"

resolveAlbumsByArtist :: AlbumRepository IO -> AlbumArgs -> ResM [AlbumQL]
resolveAlbumsByArtist repo args = gqlResolver result where
  result = findAlbumsByArtist repo (ArtistName $ AlbumArgs.name args) <&> \case
    [] -> Left "No hits"
    xs -> Right $ toAlbumQL <$> xs

resolveQuery :: Deps -> Query
resolveQuery deps = Query
  { artist         = resolveArtist (D.artistRepository deps)
  , albumsByArtist = resolveAlbumsByArtist (D.albumRepository deps)
  }
