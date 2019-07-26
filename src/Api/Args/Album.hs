{-# LANGUAGE DeriveGeneric #-}

module Api.Args.Album where

import           Data.Text
import           GHC.Generics                   ( Generic )

newtype AlbumArgs = AlbumArgs
  { name   :: Text        -- Required Argument
  } deriving Generic
