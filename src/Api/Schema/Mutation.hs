{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeFamilies #-}

-- | The GraphQL schema for mutations
module Api.Schema.Mutation where

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
import           Control.Monad.Catch            ( handle )
import           Data.Morpheus.Types            ( ResM
                                                , gqlResolver
                                                )
import           Data.Text
import           GHC.Generics                   ( Generic )
import qualified Http.Client.Params            as Http
import           Service.DataLoader             ( ExistingArtistError(..)
                                                , createAlbums
                                                , createArtists
                                                )

data Mutation = Mutation
  { newArtist :: ArtistListArgs -> ResM [ArtistQL]
  , newArtistAlbums :: ArtistIdArg -> ResM [AlbumQL]
  } deriving Generic

newArtistMutation :: Deps -> ArtistListArgs -> ResM [ArtistQL]
newArtistMutation deps args =
  let artists = Http.ArtistName <$> Args.names args
      apiCall = createArtists (D.spotifyClient deps)
                              (D.artistRepository deps)
                              (D.albumRepository deps)
                              artists
      errorMsg = "Failed to create new artist"
      handler :: IO (Either String [ArtistQL])
      handler = handle
        (\ExistingArtistError -> pure (Left "Artist already exists"))
        ((\a -> Right $ toArtistQL <$> a) <$> apiCall)
  in  gqlResolver handler

newArtistAlbumsMutation :: Deps -> ArtistIdArg -> ResM [AlbumQL]
newArtistAlbumsMutation deps arg =
  let artistId = Http.ArtistId $ Args.spotifyId arg
      apiCall =
          createAlbums (D.spotifyClient deps) (D.albumRepository deps) artistId
      errorMsg = "Failed to create albums for artist"
      handler :: IO (Either String [AlbumQL])
      handler = handle
        (\ExistingArtistError -> pure (Left "Album already exists"))
        ((\a -> Right $ toAlbumQL <$> a) <$> apiCall)
  in  gqlResolver handler

resolveMutation :: Deps -> Mutation
resolveMutation deps = Mutation
  { newArtist       = newArtistMutation deps
  , newArtistAlbums = newArtistAlbumsMutation deps
  }
