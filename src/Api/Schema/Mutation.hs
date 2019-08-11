{-# LANGUAGE DeriveGeneric, OverloadedStrings, RankNTypes, ScopedTypeVariables, TypeFamilies #-}

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
import           Api.Dependencies               ( Deps )
import qualified Api.Dependencies              as D
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
newArtistMutation deps args =
  let artists = Http.ArtistName <$> Args.names args
      apiCall = createArtists (D.spotifyClient deps)
                              (D.artistRepository deps)
                              (D.albumRepository deps)
                              artists
      errorFn ExistingArtistError = "Failed to create new artist"
  in  resolver $ baseHandle apiCall toArtistQL errorFn

newArtistAlbumsMutation :: Deps -> ArtistIdArg -> IORes [AlbumQL]
newArtistAlbumsMutation deps arg =
  let artistId = Http.ArtistId $ Args.spotifyId arg
      apiCall =
          createAlbums (D.spotifyClient deps) (D.albumRepository deps) artistId
      errorFn ExistingAlbumError = "Failed to create albums for artist"
  in  resolver $ baseHandle apiCall toAlbumQL errorFn

resolveMutation :: Deps -> Mutation
resolveMutation deps = Mutation
  { newArtist       = newArtistMutation deps
  , newArtistAlbums = newArtistAlbumsMutation deps
  }
