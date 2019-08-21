{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Service.DataLoader
  ( ExistingAlbumError(..)
  , ExistingArtistError(..)
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
import           Data.List                      ( sort )
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
import           Http.Client.Response           ( AlbumItem(..)
                                                , AlbumResponse(..)
                                                , ArtistItem(..)
                                                , TrackResponse(..)
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
  :: SpotifyClient IO -> ArtistRepository IO -> [ArtistName] -> IO [Artist]
createArtists client artistRepo names = do
  verifyArtistDoesNotExist artistRepo names
  token   <- login client
  artists <- getArtistsByName client token names
  persistArtists artists artistRepo
  pure artists

createAlbums :: SpotifyClient IO -> AlbumRepository IO -> ArtistId -> IO [Album]
createAlbums client@SpotifyClient {..} repo@AlbumRepository {..} artistId = do
  verifyAlbumDoesNotExist repo artistId
  token  <- login
  albums <- getArtistAlbums token artistId
  let albumIds = AlbumId . R.albumId <$> R.albumItems albums
  tracks <- mapConcurrently (getTracksForAlbum client token) albumIds
  let sortedAlbums   = sort $ R.albumItems albums
  let sortedDuration = snd <$> sort tracks
  let albums' = uncurry toAlbum <$> sortedAlbums `zip` sortedDuration
  let artistId'      = E.ArtistSpotifyId (unArtistId artistId)
  persistAlbums albums' artistId' repo
  pure albums'

verifyArtistDoesNotExist :: ArtistRepository IO -> [ArtistName] -> IO ()
verifyArtistDoesNotExist ArtistRepository {..} names = do
  let repoNames = E.ArtistName . unArtistName <$> names
  result <- traverse findArtist repoNames
  let filtered = result >>= maybeToList
  if not (null filtered) then throwM ExistingArtistError else pure ()

verifyAlbumDoesNotExist :: AlbumRepository IO -> ArtistId -> IO ()
verifyAlbumDoesNotExist AlbumRepository {..} ArtistId {..} = do
  let spotifyId = E.ArtistSpotifyId unArtistId
  result <- findAlbumsByArtistId spotifyId
  if not (null result) then throwM ExistingAlbumError else pure ()

toSeconds :: Int -> Int
toSeconds ms = ms `div` 1000

getArtistsByName
  :: SpotifyClient IO -> AccessToken -> [ArtistName] -> IO [Artist]
getArtistsByName SpotifyClient {..} token names = do
  responses <- mapConcurrently (searchArtist token) names
  pure $ toArtist <$> (responses >>= R.artistItems . R.artistObject)

getAlbums :: SpotifyClient IO -> AccessToken -> [ArtistId] -> IO [AlbumResponse]
getAlbums SpotifyClient {..} token = mapConcurrently (getArtistAlbums token)

getTracksForAlbum
  :: SpotifyClient IO -> AccessToken -> AlbumId -> IO (AlbumId, Int)
getTracksForAlbum SpotifyClient {..} token albumId =
  let calcLength t = toSeconds . sum $ R.trackDurationMs <$> R.trackItems t
  in  (\t -> (albumId, calcLength t)) <$> getAlbumTracks token albumId

getAlbumDuration
  :: SpotifyClient IO -> AccessToken -> [AlbumId] -> IO [TrackResponse]
getAlbumDuration SpotifyClient {..} token = traverse (getAlbumTracks token)

persistAlbums :: [Album] -> E.ArtistSpotifyId -> AlbumRepository IO -> IO ()
persistAlbums [] _ _ = putStrLn "No albums to persist"
persistAlbums albums artistId AlbumRepository {..} = do
  putStrLn $ "Persisting albums: " <> show albums
  mapConcurrently_ (createAlbum artistId) albums

persistArtists :: [Artist] -> ArtistRepository IO -> IO ()
persistArtists []      _                     = putStrLn "No artists to persist"
persistArtists artists ArtistRepository {..} = do
  putStrLn $ "Persisting artists " <> show artists
  mapConcurrently_ createArtist artists

toArtist :: ArtistItem -> Artist
toArtist ArtistItem {..} = E.Artist (E.ArtistSpotifyId artistId) artistName

toAlbum :: AlbumItem -> Int -> Album
toAlbum AlbumItem {..} =
  E.Album (E.AlbumSpotifyId albumId) albumName (dateToYear albumReleaseDate)

dateToYear :: Text -> Int
dateToYear txt = fromMaybe 0 $ readMaybe (take 4 (unpack txt))
