{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j connection pool.
module Repository.Neo
  ( mkPipePool
  , showArtist
  )
where

import           Data.Default
import           Data.Pool
import           Data.Text
import           Database.Bolt
import           Repository.Artist

mkPipePool :: IO (Pool Pipe)
mkPipePool = createPool acquire release 1 3600 10 where
  acquire = connect $ def { user = "neo4j", password = "test" }
  release = close

-- HAS_ARTIST, HAS_ALBUM, HAS_SONG, FROM_ARTIST, FROM_ALBUM, HAS_GENRE, RELATED_TO (artist)
showArtist :: ArtistRepository IO -> IO ()
showArtist repo = do
  --createArtist repo (Artist "Tool" "Los Angeles, California, US")
  artist <- findArtist repo "Tool"
  print artist
