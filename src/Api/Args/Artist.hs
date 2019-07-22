{-# LANGUAGE DeriveGeneric #-}

module Api.Args.Artist where

import           Data.Text
import           GHC.Generics                   ( Generic )

data ArtistArgs = ArtistArgs
  { name   :: Text        -- Required Argument
  } deriving Generic
