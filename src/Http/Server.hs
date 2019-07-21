{-# LANGUAGE OverloadedStrings #-}

module Http.Server where

import           Api.Root                       ( gqlApi )
import           Control.Monad.IO.Class         ( liftIO )
import           Repository.Artist              ( mkArtistRepository )
import           Repository.Neo                 ( mkPipePool )
import           Web.Scotty

serve :: IO ()
serve = do
  pool       <- mkPipePool
  artistRepo <- mkArtistRepository pool
  scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi artistRepo =<< body)
