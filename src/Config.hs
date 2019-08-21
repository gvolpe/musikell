{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Config where

import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Dhall
import           GHC.Generics
import qualified GHC.IO.Encoding
import qualified System.IO

data Neo4jConfig = Neo4jConfig
  { neo4jHost :: Text
  , neo4jPort :: Natural
  , neo4jUser :: Text
  , neo4jPassword :: Text
  , neo4jSecure :: Bool
  } deriving (Generic, Show)

newtype HttpServerConfig = HttpServerConfig
  { serverPort :: Natural
  } deriving (Generic, Show)

data SpotifyConfig = SpotifyConfig
  { apiKey :: Text
  , apiUri :: Text
  , apiAuth :: Text
  } deriving Generic

data AppConfig = AppConfig
  { neo4j :: Neo4jConfig
  , httpServer :: HttpServerConfig
  , spotify :: SpotifyConfig
  } deriving (Generic, Show)

instance Show SpotifyConfig where
  show c =
    "SpotfyConfig {apiUri = \""
      <> unpack (apiUri c)
      <> "\", apiKey = [SECRET], apiAuth = "
      <> unpack (apiAuth c)
      <> "}"

instance Interpret Neo4jConfig
instance Interpret HttpServerConfig
instance Interpret SpotifyConfig
instance Interpret AppConfig

loadConfig :: IO AppConfig
loadConfig = do
  -- Needed to run dhall under nix
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  input auto "./config/app.dhall"
