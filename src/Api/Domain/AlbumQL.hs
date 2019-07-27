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
                                                , unpack
                                                )
import           GHC.Generics                   ( Generic )
import           Repository.Entity              ( Album )
import qualified Repository.Entity             as E

-- TODO: Add spotifyId
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

addHour :: Text -> Maybe (Text, Text)
addHour mins
  | length mins == 3
  = let mins'   = read (unpack mins) :: Int
        hours   = pack . show $ mins' `div` 60
        newMins = pack . show $ mins' `mod` 60
    in  Just (hours, newMins)
  | otherwise
  = Nothing

lengthFormatted :: Int -> Text
lengthFormatted x =
  let mins = pack . show $ x `div` 60
      secs = pack . show $ x `mod` 60
      noHour m = addZero m <> ":" <> addZero secs
      withHour h m = addZero h <> ":" <> noHour m
  in  case addHour mins of
        Just (h, m) -> withHour h m
        Nothing     -> noHour mins

toAlbumQL :: Album -> AlbumQL
toAlbumQL a = AlbumQL (E.albumName a)
                      (E.albumReleasedYear a)
                      (lengthFormatted $ E.albumTotalLength a)
