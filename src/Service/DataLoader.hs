{-# LANGUAGE OverloadedStrings #-}

module Service.DataLoader
  ( ExistingArtistError(..)
  , createAlbums
  , createArtists
  )
where

import           Config
import           Control.Concurrent.Async       ( mapConcurrently
                                                , mapConcurrently_
                                                )
import           Control.Monad.Catch            ( Exception
                                                , throwM
                                                )
import           Data.Functor                   ( void )
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Http.Client.Params             ( AlbumId(..)
                                                , ArtistId(..)
                                                , ArtistName(..)
                                                , AccessToken
                                                )
import qualified Http.Client.Response          as R
import           Http.Client.Response           ( AlbumItem
                                                , AlbumResponse
                                                , ArtistItem
                                                , TrackResponse
                                                )
import           Http.Client.Spotify            ( SpotifyClient(..) )
import           Repository.Album
import           Repository.Artist
import qualified Repository.Entity             as E
import           Repository.Entity              ( Album
                                                , Artist
                                                )
import           Repository.Song
import           Text.Read                      ( readMaybe )

data ExistingAlbumError = ExistingAlbumError deriving Show
data ExistingArtistError = ExistingArtistError deriving Show

instance Exception ExistingAlbumError
instance Exception ExistingArtistError

createArtists
  :: SpotifyClient IO
  -> ArtistRepository IO
  -> AlbumRepository IO
  -> [ArtistName]
  -> IO [Artist]
createArtists client artistRepo albumRepo names = do
  verifyArtistDoesNotExist artistRepo names
  token   <- login client
  artists <- getArtistsByName client token names
  persistArtists artists artistRepo
  pure artists

createAlbums :: SpotifyClient IO -> AlbumRepository IO -> ArtistId -> IO [Album]
createAlbums client albumRepo artistId = do
  token  <- login client
  albums <- getArtistAlbums client token artistId
  let albumIds = AlbumId . R.albumId <$> R.albumItems albums
  tracks <- getAlbumDuration client token albumIds
  let durations =
        (\t -> toSeconds . sum $ R.trackDurationMs <$> R.trackItems t)
          <$> tracks
  let albums'   = uncurry toAlbum <$> (R.albumItems albums `zip` durations)
  let artistId' = E.ArtistSpotifyId (unArtistId artistId)
  persistAlbums albums' artistId' albumRepo
  pure albums'

verifyArtistDoesNotExist :: ArtistRepository IO -> [ArtistName] -> IO ()
verifyArtistDoesNotExist repo names = do
  let repoNames = E.ArtistName . unArtistName <$> names
  result <- traverse (findArtist repo) repoNames
  let filtered = result >>= maybeToList
  if not (null filtered) then throwM ExistingArtistError else pure ()

verifyAlbumDoesNotExist :: AlbumRepository IO -> ArtistId -> IO ()
verifyAlbumDoesNotExist repo artistId = do
  let spotifyId = E.ArtistSpotifyId (unArtistId artistId)
  result <- findAlbumsByArtistId repo spotifyId
  if not (null result) then throwM ExistingAlbumError else pure ()

toSeconds :: Int -> Int
toSeconds ms = ms `div` 1000

getArtistsByName
  :: SpotifyClient IO -> AccessToken -> [ArtistName] -> IO [Artist]
getArtistsByName client token names = do
  responses <- mapConcurrently (searchArtist client token) names
  pure $ toArtist <$> (responses >>= R.artistItems . R.artistObject)

getAlbums :: SpotifyClient IO -> AccessToken -> [ArtistId] -> IO [AlbumResponse]
getAlbums client token = mapConcurrently (getArtistAlbums client token)

-- Cannot use mapConcurrently here as we need the results to be in order
getAlbumDuration
  :: SpotifyClient IO -> AccessToken -> [AlbumId] -> IO [TrackResponse]
getAlbumDuration client token = traverse (getAlbumTracks client token)

persistAlbums :: [Album] -> E.ArtistSpotifyId -> AlbumRepository IO -> IO ()
persistAlbums []     _        _         = putStrLn "No albums to persist"
persistAlbums albums artistId albumRepo = do
  putStrLn $ "Persisting albums: " <> show albums
  mapConcurrently_ (createAlbum albumRepo artistId) albums

persistArtists :: [Artist] -> ArtistRepository IO -> IO ()
persistArtists []      _          = putStrLn "No artists to persist"
persistArtists artists artistRepo = do
  putStrLn $ "Persisting artists " <> show artists
  mapConcurrently_ (createArtist artistRepo) artists

toArtist :: ArtistItem -> Artist
toArtist it = E.Artist (E.ArtistSpotifyId $ R.artistId it) (R.artistName it)

toAlbum :: AlbumItem -> Int -> Album
toAlbum it = E.Album (E.AlbumSpotifyId $ R.albumId it)
                     (R.albumName it)
                     (dateToYear $ R.albumReleaseDate it)

dateToYear :: Text -> Int
dateToYear txt = fromMaybe 0 $ readMaybe (take 4 (unpack txt))
