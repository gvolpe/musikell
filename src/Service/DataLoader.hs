{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Service.DataLoader
  ( createArtistBulk
  , loadDataApp
  )
where

import           Config
import           Control.Concurrent.Async       ( mapConcurrently
                                                , mapConcurrently_
                                                )
import           Data.Functor                   ( void )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Http.Client.Params             ( ArtistId(..)
                                                , ArtistName(..)
                                                , AccessToken
                                                )
import qualified Http.Client.Response          as R
import           Http.Client.Response           ( AlbumItem
                                                , AlbumResponse
                                                , ArtistItem
                                                )
import           Http.Client.Spotify            ( getArtistAlbums
                                                , login
                                                , searchArtist
                                                )
import           Repository.Album
import           Repository.Artist
import           Repository.Entity       hiding ( ArtistId
                                                , ArtistName
                                                )
import           Repository.Song
import           Text.Read                      ( readMaybe )

loadDataApp
  :: SpotifyConfig -> ArtistRepository IO -> AlbumRepository IO -> IO ()
loadDataApp c a b = void $ createArtistBulk c a b artistNames

createArtistBulk
  :: SpotifyConfig
  -> ArtistRepository IO
  -> AlbumRepository IO
  -> [ArtistName]
  -> IO [Artist]
createArtistBulk cfg artistRepo albumRepo names = do
  token   <- login cfg
  artists <- getArtistsByName cfg token names
  let ids = ArtistId . artistSpotifyId <$> artists
  responses <- getAlbums cfg token ids
  persistData (artists `zip` responses) artistRepo albumRepo
  pure artists

getArtistsByName :: SpotifyConfig -> AccessToken -> [ArtistName] -> IO [Artist]
getArtistsByName cfg token names = do
  responses <- mapConcurrently (searchArtist cfg token) names
  pure $ toArtist <$> (responses >>= R.artistItems . R.artistObject)

getAlbums :: SpotifyConfig -> AccessToken -> [ArtistId] -> IO [AlbumResponse]
getAlbums cfg token = mapConcurrently (getArtistAlbums cfg token)

persistData
  :: [(Artist, AlbumResponse)]
  -> ArtistRepository IO
  -> AlbumRepository IO
  -> IO ()
persistData (x : xs) artistRepo albumRepo =
  createArtist artistRepo (fst x) >>= \case
    Just artistId -> do
      putStrLn $ "Persisting albums of " <> show (fst x)
      let albums = toAlbum <$> R.albumItems (snd x)
      mapConcurrently_ (createAlbum albumRepo artistId) albums
      persistData xs artistRepo albumRepo
    Nothing -> putStrLn "No artist"
persistData [] _ _ = putStrLn "Nothing else to persist"

toArtist :: ArtistItem -> Artist
toArtist it = Artist (R.artistName it) (R.artistId it)

-- TODO: Calculate totalLength from the album's tracks (need to implement this on the Spotify client)
toAlbum :: AlbumItem -> Album
toAlbum it = Album (R.albumName it) (dateToYear $ R.albumReleaseDate it) 3456

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
