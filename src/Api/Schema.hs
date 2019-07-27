{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

-- | The GraphQL schema
module Api.Schema where

import           Api.Args.Album                 ( AlbumArgs )
import qualified Api.Args.Album                as AlbumArgs
import           Api.Args.Artist                ( ArtistArgs
                                                , ArtistListArgs
                                                )
import qualified Api.Args.Artist               as Args
import           Api.Dependencies               ( Deps )
import qualified Api.Dependencies              as D
import           Api.Domain.AlbumQL             ( AlbumQL
                                                , toAlbumQL
                                                )
import           Api.Domain.ArtistQL            ( ArtistQL
                                                , toArtistQL
                                                )
import           Control.Monad.Catch            ( handle )
import           Data.Functor                   ( (<&>) )
import           Data.Morpheus.Kind             ( KIND
                                                , OBJECT
                                                )
import           Data.Morpheus.Types            ( GQLType(..)
                                                , ResM
                                                , gqlResolver
                                                )
import           Data.Text
import           GHC.Generics                   ( Generic )
import qualified Http.Client.Params            as Http
import           Repository.Album
import           Repository.Artist
import           Repository.Entity              ( Artist(..)
                                                , ArtistName(..)
                                                )
import qualified Repository.Entity             as E
import           Service.DataLoader             ( ExistingArtistError(..)
                                                , createArtistBulk
                                                )

data Query = Query
  { artist :: ArtistArgs -> ResM ArtistQL
  , albumsByArtist :: AlbumArgs -> ResM [AlbumQL]
  } deriving Generic

newtype Mutation = Mutation
  { newArtist :: ArtistListArgs -> ResM [ArtistQL]
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

newArtistMutation :: Deps -> ArtistListArgs -> ResM [ArtistQL]
newArtistMutation deps args =
  let artists = Http.ArtistName <$> Args.names args
      apiCall = createArtistBulk (D.spotifyClient deps)
                                 (D.artistRepository deps)
                                 (D.albumRepository deps)
                                 artists
      errorMsg = "Failed to create new artist"
      handler :: IO (Either String [ArtistQL])
      handler = handle
        (\ExistingArtistError -> pure (Left "Artist already exists"))
        (apiCall <&> (\a -> Right $ toArtistQL <$> a))
  in  gqlResolver handler

resolveQuery :: AlbumRepository IO -> ArtistRepository IO -> Query
resolveQuery albumRepo artistRepo = Query
  { artist         = resolveArtist artistRepo
  , albumsByArtist = resolveAlbumsByArtist albumRepo
  }

resolveMutation :: Deps -> Mutation
resolveMutation deps = Mutation { newArtist = newArtistMutation deps }
