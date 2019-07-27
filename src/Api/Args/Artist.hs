{-# LANGUAGE DeriveGeneric #-}

module Api.Args.Artist where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype ArtistArgs = ArtistArgs
  { name :: Text
  } deriving (Generic, Show)

newtype ArtistIdArg = ArtistIdArg
  { spotifyId :: Text
  } deriving (Generic, Show)

newtype ArtistListArgs = ArtistListArgs
  { names :: [Text]
  } deriving (Generic, Show)
