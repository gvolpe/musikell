{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

module Service.DataLoader where

import           Config
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
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

program :: SpotifyConfig -> ArtistRepository IO -> AlbumRepository IO -> IO ()
program cfg artistRepo albumRepo = do
  responses <- spotifyCall cfg
  persistData (artists `zip` responses) artistRepo albumRepo

spotifyCall :: SpotifyConfig -> IO [AlbumResponse]
spotifyCall cfg = do
  token <- login cfg
  print token
  traverse (getArtistAlbums cfg token) ids

persistData
  :: [(Artist, AlbumResponse)]
  -> ArtistRepository IO
  -> AlbumRepository IO
  -> IO ()
persistData (x : xs) artistRepo albumRepo = do
  createArtist artistRepo (fst x) >>= \case
    Just artistId -> do
      putStrLn $ "Persisting albums of " ++ show (fst x)
      -- traverse in parallel
      traverse_ (createAlbum albumRepo artistId) (respToAlbum $ snd x)
      persistData xs artistRepo albumRepo
    Nothing -> putStrLn "No artist"
persistData [] _ _ = putStrLn "Nothing else to persist"

dateToYear :: Text -> Int
dateToYear txt = fromMaybe 0 $ readMaybe (take 4 (show txt))

toAlbum :: AlbumItem -> Album
toAlbum it = Album (R.albumName it) (dateToYear $ R.albumReleaseDate it) 3000 -- TODO: Take this data from Spotify

respToAlbum :: AlbumResponse -> [Album]
respToAlbum resp = toAlbum <$> R.items resp

apc = ArtistId "4DFhHyjvGYa9wxdHUjtDkc"
contortionist = ArtistId "7nCgNmfYJcsVy3vOOzExYS"
davidmaximmicic = ArtistId "0wQa1N4q3HmLwxqkpVcYhs"
dreamTheater = ArtistId "2aaLAng2L2aWD2FClzwiep"
earthside = ArtistId "6mRDRKsNautYuxybddnvgg"
leprous = ArtistId "4lgrzShsg2FLA89UM2fdO5"
opeth = ArtistId "0ybFZ2Ab08V8hueghSXm6E"
persefone = ArtistId "4wxyib7wQwVxwKNFBmOhAw"
porcupineTree = ArtistId "5NXHXK6hOCotCF8lvGM1I0"
riverside = ArtistId "5yjbUO1Jocui7RKE30zfLT"

ids :: [ArtistId]
ids =
  [ apc
  , contortionist
  , davidmaximmicic
  , dreamTheater
  , earthside
  , leprous
  , opeth
  , persefone
  , porcupineTree
  , riverside
  ]

artists :: [Artist]
artists =
  [ Artist "A Perfect Circle"  "Los Angeles, California, US"
  , Artist "The Contortionist" "Indianapolis, Indiana, US"
  , Artist "David Maxim Micic" "Dubrovnik, Croatia"
  , Artist "Dream Theater"     "Boston, Massachusetts, US"
  , Artist "Earthside"         "New England, US"
  , Artist "Leprous"           "Notodden, Norway"
  , Artist "Opeth"             "Stockholm, Sweden"
  , Artist "Persefone"         "Andorra"
  , Artist "Porcupine Tree"    "Hemel Hempstead, UK"
  , Artist "Riverside"         "Warsaw, Poland"
  ]
