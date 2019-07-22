module Main where

import           Config                         ( AppConfig(..)
                                                , loadConfig
                                                )
import           Http.Server                    ( serve )
import           Repository.Album
import           Repository.Artist
import           Repository.Song
import           Repository.Neo

program :: IO ()
program = do
  config     <- loadConfig
  pool       <- mkPipePool (neo4j config)
  songRepo   <- mkSongRepository pool
  albumRepo  <- mkAlbumRepository pool
  artistRepo <- mkArtistRepository pool
  --createData artistRepo albumRepo songRepo
  showArtist artistRepo
  --showAlbum albumRepo
  showArtistAlbums albumRepo
  showAlbumSongs songRepo

main :: IO ()
main = loadConfig >>= serve
