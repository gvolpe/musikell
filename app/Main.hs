module Main where

import           Api.Dependencies               ( Deps(..) )
import           Config                         ( AppConfig(..)
                                                , loadConfig
                                                )
import           Http.Client.Spotify            ( mkSpotifyClient )
import           Http.Server                    ( serve )
import           Repository.Album               ( mkAlbumRepository )
import           Repository.Artist              ( mkArtistRepository )
import           Repository.Neo                 ( mkPipePool )

main :: IO ()
main = do
  c             <- loadConfig
  pool          <- mkPipePool (neo4j c)
  artistRepo    <- mkArtistRepository pool
  albumRepo     <- mkAlbumRepository pool
  spotifyClient <- mkSpotifyClient (spotify c)
  let deps = Deps artistRepo albumRepo spotifyClient
  serve (httpServer c) deps
