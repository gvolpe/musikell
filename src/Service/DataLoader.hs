{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Service.DataLoader
  ( ExistingArtistError(..)
  , createArtistBulk
  , loadDataApp
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

loadDataApp
  :: SpotifyClient IO -> ArtistRepository IO -> AlbumRepository IO -> IO ()
loadDataApp c a b = void $ createArtistBulk c a b artistNames

data ExistingArtistError = ExistingArtistError deriving Show

instance Exception ExistingArtistError

verifyArtistDoesNotExist :: ArtistRepository IO -> [ArtistName] -> IO ()
verifyArtistDoesNotExist repo names = do
  let repoNames = E.ArtistName . unArtistName <$> names
  result <- traverse (findArtist repo) repoNames
  let filtered = result >>= maybeToList
  if not (null filtered) then throwM ExistingArtistError else pure ()

createArtistBulk
  :: SpotifyClient IO
  -> ArtistRepository IO
  -> AlbumRepository IO
  -> [ArtistName]
  -> IO [Artist]
createArtistBulk client artistRepo albumRepo names = do
  verifyArtistDoesNotExist artistRepo names
  token   <- login client
  artists <- getArtistsByName client token names
  let ids = ArtistId . E.artistSpotifyId <$> artists
  albums <- getAlbums client token ids
  let albumIds = AlbumId . R.albumId <$> (albums >>= R.albumItems)
  tracks <- getAlbumDuration client token albumIds
  let durations = (\t -> toSeconds . sum $ R.trackDurationMs <$> R.trackItems t) <$> tracks
  print durations
  let albums' = (\a -> uncurry toAlbum <$> R.albumItems a `zip` durations) <$> albums
  print albums'
  persistData (artists `zip` albums') artistRepo albumRepo
  pure artists

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

persistData
  :: [(Artist, [Album])]
  -> ArtistRepository IO
  -> AlbumRepository IO
  -> IO ()
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
toAlbum it = E.Album (R.albumName it) (dateToYear $ R.albumReleaseDate it)

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
