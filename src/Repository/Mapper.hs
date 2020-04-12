{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | The Neo4j Mapper type class, its instances and some helper functions.
module Repository.Mapper where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( listToMaybe , maybeToList
                                                )
import           Data.Text
import           Database.Bolt
import           Repository.Entity

type NodeProps = Map Text Value

class NodeMapper a where
  toEntity :: NodeProps -> Maybe a

instance NodeMapper Artist where
  toEntity p =
    Artist
      .   ArtistSpotifyId
      <$> (Map.lookup "spotifyId" p >>= exactMaybe :: Maybe Text)
      <*> (Map.lookup "name" p >>= exactMaybe :: Maybe Text)

instance NodeMapper Album where
  toEntity p =
    Album
      .   AlbumSpotifyId
      <$> (Map.lookup "spotifyId" p >>= exactMaybe :: Maybe Text)
      <*> (Map.lookup "name" p >>= exactMaybe :: Maybe Text)
      <*> (Map.lookup "released" p >>= exactMaybe :: Maybe Int)
      <*> (Map.lookup "length" p >>= exactMaybe :: Maybe Int)

instance NodeMapper Song where
  toEntity p =
    Song
      <$> (Map.lookup "no" p >>= exactMaybe :: Maybe Int)
      <*> (Map.lookup "title" p >>= exactMaybe :: Maybe Text)
      <*> (Map.lookup "duration" p >>= exactMaybe :: Maybe Int)

--toNode :: Text -> Record -> BoltActionT Maybe Node
--toNode identifier record = record `at` identifier >>= exactMaybe

--toNodeProps :: Text -> Record -> Maybe NodeProps
--toNodeProps identifier r = nodeProps <$> toNode identifier r

toNodeProps :: Text -> Record -> Maybe NodeProps
toNodeProps identifier record = do
  nodes <- record `at` identifier
  _foo
  --nodeProps node `at` identifier r

toEntityMaybe :: NodeMapper a => Text -> [Record] -> Maybe a
toEntityMaybe identifier records =
  listToMaybe records >>= toNodeProps identifier >>= toEntity

toEntityList :: NodeMapper a => Text -> [Record] -> [a]
toEntityList identifier records = records >>= maybeToList . f
  where f r = (toNodeProps identifier r :: Maybe NodeProps) >>= toEntity

convert
  :: forall a b . RecordValue a => Text -> (a -> b) -> NodeProps -> Maybe b
convert key f p = f <$> (Map.lookup key p >>= exactMaybe :: Maybe a)

toArtistSpotifyId :: NodeProps -> Maybe ArtistSpotifyId
toArtistSpotifyId = convert "a.spotifyId" ArtistSpotifyId

toAlbumSpotifyId :: NodeProps -> Maybe AlbumSpotifyId
toAlbumSpotifyId = convert "b.spotifyId" AlbumSpotifyId
