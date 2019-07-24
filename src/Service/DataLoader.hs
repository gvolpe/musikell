{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Service.DataLoader
  ( loadData
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
import           Http.Client.Params             ( ArtistId(..) )
import qualified Http.Client.Response          as R
import           Http.Client.Response           ( AlbumItem
                                                , AlbumResponse
                                                )
import           Http.Client.Spotify            ( getArtistAlbums
                                                , login
                                                )
import           Repository.Album
import           Repository.Artist
import           Repository.Entity       hiding ( ArtistId )
import           Repository.Song
import           Text.Read                      ( readMaybe )

loadData :: SpotifyConfig -> ArtistRepository IO -> AlbumRepository IO -> IO ()
loadData cfg artistRepo albumRepo = do
  responses <- spotifyCall cfg
  persistData ((fst <$> artists) `zip` responses) artistRepo albumRepo

spotifyCall :: SpotifyConfig -> IO [AlbumResponse]
spotifyCall cfg = do
  token <- login cfg
  print token
  mapConcurrently (getArtistAlbums cfg token) (snd <$> artists)

persistData
  :: [(Artist, AlbumResponse)]
  -> ArtistRepository IO
  -> AlbumRepository IO
  -> IO ()
persistData (x : xs) artistRepo albumRepo =
  createArtist artistRepo (fst x) >>= \case
    Just artistId -> do
      putStrLn $ "Persisting albums of " <> show (fst x)
      mapConcurrently_ (createAlbum albumRepo artistId) (respToAlbum $ snd x)
      persistData xs artistRepo albumRepo
    Nothing -> putStrLn "No artist"
persistData [] _ _ = putStrLn "Nothing else to persist"

dateToYear :: Text -> Int
dateToYear txt = fromMaybe 0 $ readMaybe (take 4 (unpack txt))

toAlbum :: AlbumItem -> Album
toAlbum it = Album (R.albumName it) (dateToYear $ R.albumReleaseDate it) 3000 -- TODO: Take this data from Spotify

respToAlbum :: AlbumResponse -> [Album]
respToAlbum resp = toAlbum <$> R.items resp

artists :: [(Artist, ArtistId)]
artists =
  [ (Artist "A Perfect Circle"  "Los Angeles, California, US", ArtistId "4DFhHyjvGYa9wxdHUjtDkc")
  , (Artist "The Contortionist" "Indianapolis, Indiana, US", ArtistId "7nCgNmfYJcsVy3vOOzExYS")
  , (Artist "David Maxim Micic" "Dubrovnik, Croatia", ArtistId "0wQa1N4q3HmLwxqkpVcYhs")
  , (Artist "Dream Theater"     "Boston, Massachusetts, US", ArtistId "2aaLAng2L2aWD2FClzwiep")
  , (Artist "Earthside"         "New England, US", ArtistId "6mRDRKsNautYuxybddnvgg")
  , (Artist "Leprous"           "Notodden, Norway", ArtistId "4lgrzShsg2FLA89UM2fdO5")
  , (Artist "Opeth"             "Stockholm, Sweden", ArtistId "0ybFZ2Ab08V8hueghSXm6E")
  , (Artist "Persefone"         "Andorra", ArtistId "4wxyib7wQwVxwKNFBmOhAw")
  , (Artist "Porcupine Tree"    "Hemel Hempstead, UK", ArtistId "5NXHXK6hOCotCF8lvGM1I0")
  , (Artist "Riverside"         "Warsaw, Poland", ArtistId "5yjbUO1Jocui7RKE30zfLT")
  ]
