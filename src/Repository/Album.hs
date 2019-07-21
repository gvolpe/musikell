{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}

-- | The Neo4j repository for Album, including interface and cypher queries.
module Repository.Album
  ( mkAlbumRepository
  , AlbumRepository(..)
  )
where

import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Maybe                     ( fromMaybe )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Domain
import           Repository.Mapper
import           Repository.Song
import           Utils                          ( headMaybe )

data AlbumRepository m = AlbumRepository
  { findAlbum :: Text -> m (Maybe Album)
  , createAlbum :: Album -> [Song] -> m ()
  }

mkAlbumRepository :: Pool Pipe -> SongRepository IO -> IO (AlbumRepository IO)
mkAlbumRepository pool songRepo = pure $ AlbumRepository
  { findAlbum   = withResource pool . findAlbum'
  , createAlbum = \a songs -> do
                    withResource pool (createAlbum' a) >>= \case
                      Just nodeId -> traverse_ (createSong songRepo) songs
                      Nothing     -> error "Failed to create album" -- FIXME: throw custom exception
  }

findAlbum' :: Text -> Pipe -> IO (Maybe Album)
findAlbum' t pipe = do
  records <- run pipe $ queryP
    "MATCH (a:Album) WHERE a.name CONTAINS {title} RETURN a"
    (fromList [("title", T t)])
  pure $ headMaybe records >>= toNodeProps >>= toEntity

createAlbum' :: Album -> Pipe -> IO (Maybe NodeId)
createAlbum' a pipe = do
  records <- run pipe $ queryP
    "CREATE (a:Album { name : {name}, released : {released}, length : {length} }) RETURN ID(a)"
    (fromList
      [ ("name"    , T (albumName a))
      , ("released", I (albumReleasedYear a))
      , ("length"  , I (albumTotalLength a))
      ]
    )
  pure $ headMaybe records >>= toNodeId
