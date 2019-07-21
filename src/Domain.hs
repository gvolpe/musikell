-- | The Domain model.
module Domain where

import           Data.Text

data Artist = Artist
  { artistName :: Text
  , artistOrigin :: Text
  } deriving Show

data Album = Album
  { albumName :: Text
  , albumReleased :: Int -- epoch: see what date library to picl
  , albumTotalLength :: Int
  , albumStudio :: Maybe Text
  } deriving Show

data Song = Song
  { songNo :: Int
  , songTitle :: Text
  , songDuration :: Int
  } deriving Show
