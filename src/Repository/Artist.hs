{-# LANGUAGE OverloadedStrings #-}

module Repository.Artist where

import           Data.Functor                   ( void )
import           Data.Monoid                    ( (<>) )
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Domain
import           Repository.Mapper
import           Utils                          ( headMaybe )

data ArtistRepository m = ArtistRepository
  { findArtist :: Text -> m (Maybe Artist)
  , createArtist :: Artist -> m ()
  }

mkArtistRepository :: Pool Pipe -> IO (ArtistRepository IO)
mkArtistRepository pool = pure $ ArtistRepository
  { findArtist   = \n -> withResource pool (findArtist' n)
  , createArtist = \a -> withResource pool (createArtist' a)
  }

findArtist' :: Text -> Pipe -> IO (Maybe Artist)
findArtist' n pipe = do
  records <- run pipe
    $ query ("MATCH (a:Artist) WHERE a.name CONTAINS '" <> n <> "' RETURN a")
  pure $ headMaybe records >>= toNodeProps >>= toEntity

createArtist' :: Artist -> Pipe -> IO ()
createArtist' a pipe = void . run pipe $ query
  (  "CREATE (a:Artist { name : '"
  <> artistName a
  <> "', origin : '"
  <> artistOrigin a
  <> "' } ) RETURN ID(a)"
  )
