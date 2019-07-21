{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j repository for Album, including interface and cypher queries.
module Repository.Album
  ( mkAlbumRepository
  , AlbumRepository(..)
  )
where

import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Maybe                     ( fromMaybe )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Domain
import           Repository.Mapper
import           Utils                          ( headMaybe )

data AlbumRepository m = AlbumRepository
  { findAlbum :: Text -> m (Maybe Album)
  , createAlbum :: Album -> m ()
  }

mkAlbumRepository :: Pool Pipe -> IO (AlbumRepository IO)
mkAlbumRepository pool = pure $ AlbumRepository
  { findAlbum   = withResource pool . findAlbum'
  , createAlbum = withResource pool . createAlbum'
  }

findAlbum' :: Text -> Pipe -> IO (Maybe Album)
findAlbum' t pipe = do
  records <- run pipe $ queryP
    "MATCH (a:Album) WHERE a.title CONTAINS {title} RETURN a"
    (fromList [("title", T t)])
  pure $ headMaybe records >>= toNodeProps >>= toEntity

createAlbum' :: Album -> Pipe -> IO ()
createAlbum' a pipe = void . run pipe $ queryP
  "CREATE (a:Album { name : {name}, released : {released}, length : {length}, studio : {studio} }) RETURN ID(a)"
  (fromList
    [ ("name"    , T (albumName a))
    , ("released", I (albumReleased a))
    , ("length"  , I (albumTotalLength a))
    , ("studio"  , T (fromMaybe "" $ albumStudio a))
    ]
  )
