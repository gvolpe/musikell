{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Http.Server where

import           Api.Dependencies               ( Deps )
import           Api.Root                       ( gqlApi
                                                , gqlDoc
                                                )
import           Config                         ( HttpServerConfig(..) )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( unpack )
import           GHC.Natural                    ( naturalToInt )
import           Web.Scotty

serve :: HttpServerConfig -> Deps -> IO ()
serve HttpServerConfig {..} deps = scotty port $ do
  post "/api" $ raw =<< (liftIO . gqlApi deps =<< body)
  get "/schema.gql" $ raw (gqlDoc deps)
  where port = naturalToInt serverPort
