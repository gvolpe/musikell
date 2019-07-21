{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j repository for Album, including interface and cypher queries.
module Repository.Album
  ( mkAlbumRepository
  , AlbumRepository(..)
  )
where

import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Monoid                    ( (<>) )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Domain
import           Repository.Mapper
import           Repository.Song
import           Utils                          ( headMaybe )

data AlbumRepository m = AlbumRepository
  { findAlbum :: AlbumName -> m (Maybe Album)
  , findAlbumsByArtist :: ArtistName -> m [Album]
  , createAlbum :: ArtistId -> Album -> m (Maybe AlbumId)
  }

mkAlbumRepository :: Pool Pipe -> IO (AlbumRepository IO)
mkAlbumRepository pool = pure $ AlbumRepository
  { findAlbum          = withResource pool . findAlbum'
  , findAlbumsByArtist = withResource pool . findAlbumsByArtist'
  , createAlbum        = \artistId album ->
                           withResource pool (createAlbum' artistId album)
  }

findAlbum' :: AlbumName -> Pipe -> IO (Maybe Album)
findAlbum' a pipe = toEntityMaybe "b" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (b:Album) WHERE b.name CONTAINS {title} RETURN b"
    (fromList [("title", T (unAlbumName a))])

findAlbumsByArtist' :: ArtistName -> Pipe -> IO [Album]
findAlbumsByArtist' a pipe = toEntityList "b" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (a:Artist)-[:HAS_ALBUM]->(b:Album) WHERE a.name CONTAINS {artistName} RETURN b"
    (fromList [("artistName", T (unArtistName a))])

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
