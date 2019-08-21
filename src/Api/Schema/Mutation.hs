{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies #-}

-- | The GraphQL schema for mutations
module Api.Schema.Mutation
  ( Mutation(..)
  , resolveMutation
  )
where

import           Api.Args.Artist                ( ArtistIdArg
                                                , ArtistListArgs
                                                )
import qualified Api.Args.Artist               as Args
import           Api.Dependencies               ( Deps(..) )
import           Api.Domain.AlbumQL             ( AlbumQL
                                                , toAlbumQL
                                                )
import           Api.Domain.ArtistQL            ( ArtistQL
                                                , toArtistQL
                                                )
import           Control.Monad.Catch            ( Exception
                                                , handle
                                                )
import           Data.Morpheus.Types            ( IORes
                                                , resolver
                                                )
import           Data.Text
import           GHC.Generics                   ( Generic )
import qualified Http.Client.Params            as Http
import           Service.DataLoader             ( ExistingAlbumError(..)
                                                , ExistingArtistError(..)
                                                , createAlbums
                                                , createArtists
                                                )

data Mutation = Mutation
  { newArtist :: ArtistListArgs -> IORes [ArtistQL]
  , newArtistAlbums :: ArtistIdArg -> IORes [AlbumQL]
  } deriving Generic

baseHandle
  :: forall a b e
   . Exception e
  => IO [a]
  -> (a -> b)
  -> (e -> Text)
  -> IO (Either String [b])
baseHandle action f g =
  handle (pure . Left . unpack <$> g) ((\x -> Right $ f <$> x) <$> action)

newArtistMutation :: Deps -> ArtistListArgs -> IORes [ArtistQL]
newArtistMutation Deps {..} args =
  let artists = Http.ArtistName <$> Args.names args
      apiCall = createArtists spotifyClient artistRepository artists
      errorFn ExistingArtistError = "Failed to create new artist"
  in  resolver $ baseHandle apiCall toArtistQL errorFn

newArtistAlbumsMutation :: Deps -> ArtistIdArg -> IORes [AlbumQL]
newArtistAlbumsMutation Deps {..} arg =
  let artistId = Http.ArtistId $ Args.spotifyId arg
      apiCall = createAlbums spotifyClient albumRepository artistId
      errorFn ExistingAlbumError = "Failed to create albums for artist"
  in  resolver $ baseHandle apiCall toAlbumQL errorFn

resolveMutation :: Deps -> Mutation
resolveMutation deps = Mutation
  { newArtist       = newArtistMutation deps
  , newArtistAlbums = newArtistAlbumsMutation deps
  }
