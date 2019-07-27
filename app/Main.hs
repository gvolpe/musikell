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
import           Service.DataLoader             ( loadDataApp )

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
  client <- mkSpotifyClient (spotify c)
  token  <- login client
  print token
  let tool = ArtistName "Porcupine Tree"
  artist <- searchArtist client token tool
  print artist

p3 :: AppConfig -> IO ()
p3 c = do
  client     <- mkSpotifyClient (spotify c)
  pool       <- mkPipePool (neo4j c)
  albumRepo  <- mkAlbumRepository pool
  artistRepo <- mkArtistRepository pool
  loadDataApp client artistRepo albumRepo

main :: IO ()
main = loadConfig >>= serve
