{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

-- | The GraphQL schema
module Api.Schema where

import           Api.Args.Album                 ( AlbumArgs )
import qualified Api.Args.Album                as AlbumArgs
import           Api.Args.Artist                ( ArtistArgs )
import qualified Api.Args.Artist               as Args
import           Api.Domain.AlbumQL             ( AlbumQL
                                                , toAlbumQL
                                                )
import           Api.Domain.ArtistQL            ( ArtistQL
                                                , toArtistQL
                                                )
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
import           Repository.Album
import           Repository.Artist
import           Repository.Entity              ( Artist(..)
                                                , ArtistName(..)
                                                )
import qualified Repository.Entity             as E
import           Utils                          ( maybeToEither )

data Query = Query
  { artist :: ArtistArgs -> ResM ArtistQL
  , albumsByArtist :: AlbumArgs -> ResM [AlbumQL]
  } deriving Generic

data Mutation = Mutation
  { newArtist :: ArtistArgs -> ResM ArtistQL
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

newArtistMutation :: ArtistRepository IO -> ArtistArgs -> ResM ArtistQL
newArtistMutation repo args =
  let artist = Artist (Args.name args) "hardcoded-id"
  in  gqlResolver $ createArtist repo artist <&> \case
        Just _  -> Right $ toArtistQL artist
        Nothing -> Left "Failed to create new artist"

resolveQuery :: AlbumRepository IO -> ArtistRepository IO -> Query
resolveQuery albumRepo artistRepo = Query
  { artist         = resolveArtist artistRepo
  , albumsByArtist = resolveAlbumsByArtist albumRepo
  }

resolveMutation :: ArtistRepository IO -> Mutation
resolveMutation repo = Mutation { newArtist = newArtistMutation repo }
