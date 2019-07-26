{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, TypeFamilies #-}

module Api.Domain.AlbumQL
  ( AlbumQL(..)
  , toAlbumQL
  )
where

import           Prelude                 hiding ( length )
import           Data.Monoid                    ( (<>) )
import           Data.Morpheus.Kind             ( KIND
                                                , OBJECT
                                                )
import           Data.Morpheus.Types            ( GQLType )
import           Data.Text                      ( Text
                                                , length
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )
import           Repository.Entity              ( Album )
import qualified Repository.Entity             as E

data AlbumQL = AlbumQL
  { name :: Text
  , yearOfRelease :: Int
  , totalLength :: Text
  } deriving (Generic, GQLType)

type instance KIND AlbumQL = OBJECT

addZero :: Text -> Text
addZero x | x == "0"      = "00"
          | length x == 1 = "0" <> x
          | otherwise     = x

lengthFormatted :: Int -> Text
lengthFormatted x = addZero mins <> ":" <> addZero secs where
  mins = pack . show $ x `div` 60
  secs = pack . show $ x `mod` 60

toAlbumQL :: Album -> AlbumQL
toAlbumQL a = AlbumQL (E.albumName a)
                      (E.albumReleasedYear a)
                      (lengthFormatted $ E.albumTotalLength a)
