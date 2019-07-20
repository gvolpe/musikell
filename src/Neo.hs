{-# LANGUAGE OverloadedStrings #-}

-- HAS_ARTIST, HAS_ALBUM, HAS_SONG, FROM_ARTIST, FROM_ALBUM, HAS_GENRE, RELATED_TO (artist)
module Neo where

import           Data.Default
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Monoid                    ( (<>) )
import           Data.Text
import           Database.Bolt

data Artist = Artist
  { artistName :: Text
  , artistOrigin :: Text
  } deriving Show

type NodeProps = Map Text Value

class NodeMapper a where
  toEntity :: NodeProps -> Maybe a

instance NodeMapper Artist where
  toEntity p =
    Artist
      <$> (Map.lookup "name" p >>= exact :: Maybe Text)
      <*> (Map.lookup "origin" p >>= exact :: Maybe Text)

toNode :: Monad m => Record -> m Node
toNode record = record `at` "a" >>= exact

toNodeProps :: Monad m => Record -> m NodeProps
toNodeProps r = nodeProps <$> toNode r

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (x : _) = Just x

-- FIXME: Pipe should not be part of the interface, it's implementatoin detail
data ArtistRepository m = ArtistRepository
  { findArtist :: Pipe -> Text -> m (Maybe Artist)
  , createArtist :: Pipe -> Artist -> m ()
  }

mkArtistRepository :: IO (ArtistRepository IO)
mkArtistRepository = pure $ ArtistRepository
  { findArtist   =
    \pipe n -> do
      records <- run pipe $ query
        ("MATCH (a:Artist) WHERE a.name CONTAINS '" <> n <> "' RETURN a")
      pure $ headMaybe records >>= toNodeProps >>= toEntity
  , createArtist = \pipe a -> void . run pipe $ query
                     (  "CREATE (a:Artist { name : '"
                     <> artistName a
                     <> "', origin : '"
                     <> artistOrigin a
                     <> "' } ) RETURN ID(a)"
                     )
  }

-- TODO: Use resource-pool to create and close pipe
foo :: ArtistRepository IO -> IO ()
foo repo = do
  pipe   <- connect $ def { user = "neo4j", password = "test" }
  --createArtist repo pipe (Artist "Tool" "Los Angeles, California, US")
  artist <- findArtist repo pipe "Tool"
  print artist
  close pipe
