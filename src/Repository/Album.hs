{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j repository for Album, including interface and cypher queries.
module Repository.Album
  ( mkAlbumRepository
  , AlbumRepository(..)
  )
where

import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Domain
import           Repository.Mapper
import           Repository.Song
import           Utils                          ( headMaybe )

data AlbumRepository m = AlbumRepository
  { findAlbum :: Text -> m (Maybe Album)
  , findAlbumsByArtist :: Text -> m [Album]
  , createAlbum :: ArtistId -> Album -> m (Maybe AlbumId)
  }

mkAlbumRepository :: Pool Pipe -> IO (AlbumRepository IO)
mkAlbumRepository pool = pure $ AlbumRepository
  { findAlbum          = withResource pool . findAlbum'
  , findAlbumsByArtist = withResource pool . findAlbumsByArtist'
  , createAlbum        = \artistId album ->
                           withResource pool (createAlbum' artistId album)
  }

findAlbum' :: Text -> Pipe -> IO (Maybe Album)
findAlbum' t pipe = do
  records <- run pipe $ queryP
    "MATCH (b:Album) WHERE b.name CONTAINS {title} RETURN b"
    (fromList [("title", T t)])
  pure $ headMaybe records >>= toNodeProps "b" >>= toEntity

findAlbumsByArtist' :: Text -> Pipe -> IO [Album]
findAlbumsByArtist' artistName pipe = do
  records <- run pipe $ queryP
    "MATCH (a:Artist)-[:HAS_ALBUM]->(b:Album) WHERE a.name CONTAINS {artistName} RETURN b"
    (fromList [("artistName", T artistName)])
  pure $ records >>= (\r -> maybeToList ((toNodeProps "b" r :: Maybe NodeProps) >>= toEntity))

createAlbum' :: ArtistId -> Album -> Pipe -> IO (Maybe AlbumId)
createAlbum' artistId a pipe = do
  records <- run pipe $ queryP
    (  "MATCH (a:Artist) WHERE ID(a)={artistId} "
    <> "CREATE (b:Album { name : {name}, released : {released}, length : {length} }) "
    <> "CREATE (a)-[h:HAS_ALBUM]->(b) "
    <> "CREATE (b)-[f:FROM_ARTIST]->(a) "
    <> "RETURN ID(b)"
    )
    (fromList
      [ ("artistId", I (unArtistId artistId))
      , ("name"    , T (albumName a))
      , ("released", I (albumReleasedYear a))
      , ("length"  , I (albumTotalLength a))
      ]
    )
  pure $ headMaybe records >>= toAlbumId
