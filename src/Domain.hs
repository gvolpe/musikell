-- | The Domain model.
module Domain where

import           Data.Text

data Artist = Artist
  { artistName :: Text
  , artistOrigin :: Text
  } deriving Show
