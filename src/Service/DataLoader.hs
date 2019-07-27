{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

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

data ExistingArtistError = ExistingArtistError deriving Show

instance Exception ExistingArtistError

verifyArtistDoesNotExist :: ArtistRepository IO -> [ArtistName] -> IO ()
verifyArtistDoesNotExist repo names = do
  let repoNames = E.ArtistName . unArtistName <$> names
  result <- traverse (findArtist repo) repoNames
  let filtered = result >>= maybeToList
  if not (null filtered) then throwM ExistingArtistError else pure ()

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
  let durations = (\t -> toSeconds . sum $ R.trackDurationMs <$> R.trackItems t) <$> tracks
  let albums' = uncurry toAlbum <$> (R.albumItems albums `zip` durations)
  let artistId' = E.SpotifyId (unArtistId artistId)
  persistAlbums albums' artistId' albumRepo
  pure albums'

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

persistAlbums :: [Album] -> E.SpotifyId -> AlbumRepository IO -> IO ()
persistAlbums []     _        _         = putStrLn "No albums to persist"
persistAlbums albums artistId albumRepo = do
  putStrLn $ "Persisting albums: " <> show albums
  mapConcurrently_ (createAlbum albumRepo artistId) albums

persistArtists :: [Artist] -> ArtistRepository IO -> IO ()
persistArtists []      _          = putStrLn "No artists to persist"
persistArtists artists artistRepo = do
  putStrLn $ "Persisting artists " <> show artists
  mapConcurrently_ (createArtist artistRepo) artists

persistData
  :: [(Artist, [Album])] -> ArtistRepository IO -> AlbumRepository IO -> IO ()
persistData (x : xs) artistRepo albumRepo =
  createArtist artistRepo (fst x) >>= \case
    Just artistId -> do
      putStrLn $ "Persisting albums of " <> show (fst x)
      mapConcurrently_ (createAlbum albumRepo artistId) (snd x)
      persistData xs artistRepo albumRepo
    Nothing -> putStrLn "No artist"
persistData [] _ _ = putStrLn "Nothing else to persist"

toArtist :: ArtistItem -> Artist
toArtist it = E.Artist (R.artistName it) (R.artistId it)

toAlbum :: AlbumItem -> Int -> Album
toAlbum it =
  E.Album (R.albumId it) (R.albumName it) (dateToYear $ R.albumReleaseDate it)

dateToYear :: Text -> Int
dateToYear txt = fromMaybe 0 $ readMaybe (take 4 (unpack txt))

artistNames :: [ArtistName]
artistNames =
  ArtistName
    <$> [ "A Perfect Circle"
        , "The Contortionist"
        , "David Maxim Micic"
        , "Dream Theater"
        , "Earthside"
        , "Leprous"
        , "Opeth"
        , "Persefone"
        , "Porcupine Tree"
        , "Riverside"
        ]
