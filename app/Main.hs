{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config                         ( AppConfig(..)
                                                , loadConfig
                                                )
import           Http.Client.Params             ( ArtistId(..)
                                                , ArtistName(..)
                                                )
import           Http.Client.Spotify
import           Http.Server                    ( serve )
import           Repository.Album
import           Repository.Artist
import           Repository.Song
import           Repository.Neo
import           Service.DataLoader             ( loadData )

p1 :: AppConfig -> IO ()
p1 c = do
  pool       <- mkPipePool (neo4j c)
  songRepo   <- mkSongRepository pool
  albumRepo  <- mkAlbumRepository pool
  artistRepo <- mkArtistRepository pool
  showArtist artistRepo
  --showAlbum albumRepo
  showArtistAlbums albumRepo
  showAlbumSongs songRepo

p2 :: AppConfig -> IO ()
p2 c = do
  token <- login (spotify c)
  print token
  let tool = ArtistName "Porcupine Tree"
  artist <- searchArtist (spotify c) token tool
  print artist

p3 :: AppConfig -> IO ()
p3 c = do
  pool       <- mkPipePool (neo4j c)
  albumRepo  <- mkAlbumRepository pool
  artistRepo <- mkArtistRepository pool
  loadData (spotify c) artistRepo albumRepo

main :: IO ()
main = loadConfig >>= serve
