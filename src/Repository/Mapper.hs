{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j Mapper type class, its instances and some helper functions.
module Repository.Mapper where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( maybeToList )
import           Data.Text
import           Database.Bolt
import           Repository.Entity
import           Utils                          ( headMaybe )

type NodeProps = Map Text Value

class NodeMapper a where
  toEntity :: NodeProps -> Maybe a

instance NodeMapper Artist where
  toEntity p =
    Artist
      <$> (Map.lookup "spotifyId" p >>= exact :: Maybe Text)
      <*> (Map.lookup "name" p >>= exact :: Maybe Text)

instance NodeMapper Album where
  toEntity p =
    Album
      <$> (Map.lookup "spotifyId" p >>= exact :: Maybe Text)
      <*> (Map.lookup "name" p >>= exact :: Maybe Text)
      <*> (Map.lookup "released" p >>= exact :: Maybe Int)
      <*> (Map.lookup "length" p >>= exact :: Maybe Int)

instance NodeMapper Song where
  toEntity p =
    Song
      <$> (Map.lookup "no" p >>= exact :: Maybe Int)
      <*> (Map.lookup "title" p >>= exact :: Maybe Text)
      <*> (Map.lookup "duration" p >>= exact :: Maybe Int)

toNode :: Monad m => Text -> Record -> m Node
toNode identifier record = record `at` identifier >>= exact

toNodeProps :: Monad m => Text -> Record -> m NodeProps
toNodeProps identifier r = nodeProps <$> toNode identifier r

toEntityMaybe :: NodeMapper a => Text -> [Record] -> Maybe a
toEntityMaybe identifier records =
  headMaybe records >>= toNodeProps identifier >>= toEntity

toEntityList :: NodeMapper a => Text -> [Record] -> [a]
toEntityList identifier records = records >>= maybeToList . f
  where f r = (toNodeProps identifier r :: Maybe NodeProps) >>= toEntity

toArtistSpotifyId :: NodeProps -> Maybe SpotifyId
toArtistSpotifyId p =
  SpotifyId <$> (Map.lookup "a.spotifyId" p >>= exact :: Maybe Text)

toAlbumSpotifyId :: NodeProps -> Maybe SpotifyId
toAlbumSpotifyId p =
  SpotifyId <$> (Map.lookup "b.spotifyId" p >>= exact :: Maybe Text)
