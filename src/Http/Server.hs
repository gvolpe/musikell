{-# LANGUAGE OverloadedStrings #-}

module Http.Server where

import           Api.Dependencies               ( Deps(..) )
import           Api.Root                       ( gqlApi
                                                , gqlDoc
                                                )
import           Config                         ( AppConfig(..)
                                                , HttpServerConfig(..)
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( unpack )
import           GHC.Natural                    ( naturalToInt )
import           Http.Client.Spotify            ( mkSpotifyClient )
import           Repository.Album               ( mkAlbumRepository )
import           Repository.Artist              ( mkArtistRepository )
import           Repository.Neo                 ( mkPipePool )
import           Web.Scotty

serve :: AppConfig -> IO ()
serve c = do
  pool          <- mkPipePool (neo4j c)
  artistRepo    <- mkArtistRepository pool
  albumRepo     <- mkAlbumRepository pool
  spotifyClient <- mkSpotifyClient (spotify c)
  let deps = Deps artistRepo albumRepo spotifyClient
  let port = naturalToInt $ serverPort (httpServer c)
  scotty port $ do
    post "/api" $ raw =<< (liftIO . gqlApi deps =<< body)
    get "/schema.gql" $ raw (gqlDoc deps)
