{-# LANGUAGE OverloadedStrings #-}

module Http.Server where

import           Api.Root                       ( gqlApi )
import           Config                         ( AppConfig(..)
                                                , HttpServerConfig(..)
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( unpack )
import           GHC.Natural                    ( naturalToInt )
import           Repository.Artist              ( mkArtistRepository )
import           Repository.Neo                 ( mkPipePool )
import           Web.Scotty

serve :: AppConfig -> IO ()
serve c = do
  pool       <- mkPipePool (neo4j c)
  artistRepo <- mkArtistRepository pool
  let port = naturalToInt $ serverPort (httpServer c)
  scotty port $ post "/api" $ raw =<< (liftIO . gqlApi artistRepo =<< body)
