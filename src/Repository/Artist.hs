{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- | The Neo4j repository for Artist, including interface and cypher queries.
module Repository.Artist
  ( mkArtistRepository
  , ArtistRepository(..)
  )
where

import           Data.Functor                   ( (<&>)
                                                , void
                                                )
import           Data.Map                       ( fromList )
import           Data.Maybe                     ( listToMaybe )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Repository.Entity
import           Repository.Mapper

data ArtistRepository m = ArtistRepository
  { findArtist :: ArtistName -> m (Maybe Artist)
  , createArtist :: Artist -> m (Maybe ArtistSpotifyId)
  }

mkArtistRepository :: Pool Pipe -> IO (ArtistRepository IO)
mkArtistRepository pool = pure $ ArtistRepository
  { findArtist   = withResource pool . findArtist'
  , createArtist = withResource pool . createArtist'
  }

findArtist' :: ArtistName -> Pipe -> IO (Maybe Artist)
findArtist' ArtistName {..} pipe = toEntityMaybe "a" <$> stmt where
  stmt = run pipe $ queryP
    "MATCH (a:Artist) WHERE a.name CONTAINS {name} RETURN a"
    (fromList [("name", T unArtistName)])

createArtist' :: Artist -> Pipe -> IO (Maybe ArtistSpotifyId)
createArtist' Artist {..} pipe = do
  records <- run pipe $ queryP
    "CREATE (a:Artist { name : {name}, spotifyId : {spotifyId} }) RETURN a.spotifyId"
    (fromList
      [ ("name"     , T artistName)
      , ("spotifyId", T (unArtistSpotifyId artistSpotifyId))
      ]
    )
  pure $ listToMaybe records >>= toArtistSpotifyId
