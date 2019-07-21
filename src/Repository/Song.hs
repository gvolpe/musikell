{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j repository for Song, including interface and cypher queries.
module Repository.Song
  ( mkSongRepository
  , SongRepository(..)
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

data SongRepository m = SongRepository
  { findSong :: Text -> m (Maybe Song)
  , createSong :: Song -> m ()
  }

mkSongRepository :: Pool Pipe -> IO (SongRepository IO)
mkSongRepository pool = pure $ SongRepository
  { findSong   = withResource pool . findSong'
  , createSong = withResource pool . createSong'
  }

findSong' :: Text -> Pipe -> IO (Maybe Song)
findSong' t pipe = do
  records <- run pipe $ queryP
    "MATCH (s:Song) WHERE s.title CONTAINS {title} RETURN s"
    (fromList [("title", T t)])
  pure $ headMaybe records >>= toNodeProps >>= toEntity

createSong' :: Song -> Pipe -> IO ()
createSong' s pipe = void . run pipe $ queryP
  "CREATE (s:Song { no : {no}, title : {title}, duration : {duration} }) RETURN ID(s)"
  (fromList
    [ ("no"      , I (songNo s))
    , ("title"   , T (songTitle s))
    , ("duration", I (songDuration s))
    ]
  )
