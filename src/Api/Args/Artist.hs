{-# LANGUAGE DeriveGeneric #-}

module Api.Args.Artist where

import           Data.Text
import           GHC.Generics                   ( Generic )

newtype ArtistArgs = ArtistArgs
  { name :: Text        -- Required Argument
  } deriving (Generic, Show)

newtype ArtistListArgs = ArtistListArgs
  { names :: [Text]        -- Required Argument
  } deriving (Generic, Show)
