module Main where

import           Http.Server                    ( serve )
import           Repository.Album
import           Repository.Artist
import           Repository.Song
import           Repository.Neo

program :: IO ()
program = do
  pool       <- mkPipePool
  songRepo   <- mkSongRepository pool
  albumRepo  <- mkAlbumRepository pool
  artistRepo <- mkArtistRepository pool
  --createData artistRepo albumRepo songRepo
  showArtist artistRepo
  --showAlbum albumRepo
  showArtistAlbums albumRepo
  showAlbumSongs songRepo

main :: IO ()
main = serve
