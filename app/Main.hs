{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config                         ( AppConfig(..)
                                                , loadConfig
                                                )
import           Http.Client.Params             ( ArtistId(..) )
import           Http.Client.Spotify            ( getArtistAlbums
                                                , login
                                                )
import           Http.Server                    ( serve )
import           Repository.Album
import           Repository.Artist
import           Repository.Song
import           Repository.Neo

p1 :: AppConfig -> IO ()
p1 c = do
  pool       <- mkPipePool (neo4j c)
  songRepo   <- mkSongRepository pool
  albumRepo  <- mkAlbumRepository pool
  artistRepo <- mkArtistRepository pool
  --createData artistRepo albumRepo songRepo
  showArtist artistRepo
  --showAlbum albumRepo
  showArtistAlbums albumRepo
  showAlbumSongs songRepo

p2 :: AppConfig -> IO ()
p2 c = do
  token <- login (spotify c)
  print token
  let pt = ArtistId "5NXHXK6hOCotCF8lvGM1I0"
  albums <- getArtistAlbums (spotify c) token pt
  print albums

main :: IO ()
main = loadConfig >>= p2
