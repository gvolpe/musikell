{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

-- | The Neo4j connection pool.
module Repository.Neo where

import           Data.Default
import           Data.Foldable                  ( traverse_ )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Domain
import           Repository.Album
import           Repository.Artist
import           Repository.Song

-- TODO: Use configuration file to store connection details
mkPipePool :: IO (Pool Pipe)
mkPipePool = createPool acquire release 1 3600 10 where
  acquire = connect $ def { user = "neo4j", password = "test" }
  release = close

-- HAS_ARTIST, HAS_ALBUM, HAS_SONG, FROM_ARTIST, FROM_ALBUM, HAS_GENRE, RELATED_TO (artist)
showArtist :: ArtistRepository IO -> IO ()
showArtist repo = do
  artist <- findArtist repo "Tool"
  print artist

showAlbum :: AlbumRepository IO -> IO ()
showAlbum repo = do
  album <- findAlbum repo "10.000 Days"
  print album

showArtistAlbums :: AlbumRepository IO -> IO ()
showArtistAlbums repo = do
  albums <- findAlbumsByArtist repo "Tool"
  print albums

showAlbumSongs :: SongRepository IO -> IO ()
showAlbumSongs repo = do
  songs <- findSongsByAlbum repo "10.000"
  print songs

showArtistSongs :: SongRepository IO -> IO ()
showArtistSongs repo = do
  songs <- findSongsByArtist repo "Tool"
  print songs

createData
  :: ArtistRepository IO -> AlbumRepository IO -> SongRepository IO -> IO ()
createData artistRepo albumRepo songRepo =
  createArtist artistRepo (Artist "Tool" "Los Angeles, California, US")
    >>= \case
          Just artistId ->
            createAlbum albumRepo artistId (Album "10.000 Days" 2006 4545) >>= \case
              Just albumId ->
                traverse_ (createSong songRepo artistId albumId) songs
              Nothing -> pure ()
          Nothing -> pure () -- TODO: Raise error?
 where
  songs =
    [ Song 1  "Vicarious"                 426
    , Song 2  "Jambi"                     448
    , Song 3  "Wings for Marie (Pt 1)"    371
    , Song 4  "10.000 Days (Wings Pt 2)"  673
    , Song 5  "The Pot"                   381
    , Song 6  "Lipan Conjuring"           71
    , Song 7  "Lost Keys (Blame Hofmann)" 226
    , Song 8  "Rosetta Stoned"            671
    , Song 9  "Intension"                 441
    , Song 10 "Right in Two"              535
    , Song 11 "Viginti Tres"              302
    ]

