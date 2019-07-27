{-# LANGUAGE DeriveGeneric #-}

module Api.Args.Album where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype AlbumArgs = AlbumArgs
  { name   :: Text
  } deriving Generic
