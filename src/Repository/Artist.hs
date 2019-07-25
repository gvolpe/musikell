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
import           Repository.Entity
import           Repository.Mapper
import           Utils                          ( headMaybe )

data ArtistRepository m = ArtistRepository
  { findArtist :: ArtistName -> m (Maybe Artist)
  , createArtist :: Artist -> m (Maybe ArtistId)
  }

mkArtistRepository :: Pool Pipe -> IO (ArtistRepository IO)
mkArtistRepository pool = pure $ ArtistRepository
  { findArtist   = withResource pool . findArtist'
  , createArtist = withResource pool . createArtist'
  }

findArtist' :: ArtistName -> Pipe -> IO (Maybe Artist)
findArtist' a pipe = toEntityMaybe "a" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (a:Artist) WHERE a.name CONTAINS {name} RETURN a"
    (fromList [("name", T (unArtistName a))])

createArtist' :: Artist -> Pipe -> IO (Maybe ArtistId)
createArtist' a pipe = do
  records <- run pipe $ queryP
    "CREATE (a:Artist { name : {name}, spotifyId : {spotifyId} }) RETURN ID(a)"
    (fromList [("name", T (artistName a)), ("spotifyId", T (artistSpotifyId a))])
  pure $ headMaybe records >>= toArtistId
