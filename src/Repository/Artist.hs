{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j repository for Artist, including interface and cypher queries.
module Repository.Artist
  ( mkArtistRepository
  , ArtistRepository(..)
  )
where

import           Data.Functor                   ( void )
import           Data.Map                       ( fromList )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Domain
import           Repository.Mapper
import           Utils                          ( headMaybe )

data ArtistRepository m = ArtistRepository
  { findArtist :: Text -> m (Maybe Artist)
  , createArtist :: Artist -> m (Maybe ArtistId)
  }

mkArtistRepository :: Pool Pipe -> IO (ArtistRepository IO)
mkArtistRepository pool = pure $ ArtistRepository
  { findArtist   = withResource pool . findArtist'
  , createArtist = withResource pool . createArtist'
  }

findArtist' :: Text -> Pipe -> IO (Maybe Artist)
findArtist' n pipe = do
  records <- run pipe $ queryP
    "MATCH (a:Artist) WHERE a.name CONTAINS {name} RETURN a"
    (fromList [("name", T n)])
  pure $ headMaybe records >>= toNodeProps >>= toEntity

createArtist' :: Artist -> Pipe -> IO (Maybe ArtistId)
createArtist' a pipe = do
  records <- run pipe $ queryP
    "CREATE (a:Artist { name : {name}, origin : {origin} }) RETURN ID(a)"
    (fromList [("name", T (artistName a)), ("origin", T (artistOrigin a))])
  pure $ headMaybe records >>= toArtistId
