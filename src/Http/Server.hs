{-# LANGUAGE OverloadedStrings #-}

module Http.Server where

import           Api.Dependencies               ( Deps(..) )
import           Api.Root                       ( gqlApi )
import           Config                         ( AppConfig(..)
                                                , HttpServerConfig(..)
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( unpack )
import           GHC.Natural                    ( naturalToInt )
import           Repository.Album               ( mkAlbumRepository )
import           Repository.Artist              ( mkArtistRepository )
import           Repository.Neo                 ( mkPipePool )
import           Web.Scotty

serve :: AppConfig -> IO ()
serve c = do
  pool       <- mkPipePool (neo4j c)
  artistRepo <- mkArtistRepository pool
  albumRepo  <- mkAlbumRepository pool
  let deps = Deps artistRepo albumRepo
  let port = naturalToInt $ serverPort (httpServer c)
  scotty port $ post "/api" $ raw =<< (liftIO . gqlApi deps =<< body)
