{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, ViewPatterns #-}

module Api.Domain.AlbumQL
  ( AlbumQL(..)
  , toAlbumQL
  )
where

import           Prelude                 hiding ( length )
import           Data.Monoid                    ( (<>) )
import           Data.Morpheus.Kind             ( OBJECT )
import           Data.Morpheus.Types            ( GQLType(..) )
import           Data.Text                      ( Text
                                                , length
                                                , pack
                                                , unpack
                                                )
import           GHC.Generics                   ( Generic )
import           Refined
import           Repository.Entity              ( Album )
import qualified Repository.Entity             as E
import           Text.Read                      ( readMaybe )

data AlbumQL = AlbumQL
  { spotifyId :: Text
  , name :: Text
  , yearOfRelease :: Int
  , totalLength :: Text
  } deriving Generic

instance GQLType AlbumQL where
  type KIND AlbumQL = OBJECT

type PosInt = Refined Positive Int

maybeAddZero :: Text -> Text
maybeAddZero x | x == "0"      = "00"
               | length x == 1 = "0" <> x
               | otherwise     = x

addHour :: Text -> Maybe (Text, Text)
addHour mins
  | length mins == 3
  = let mins'   = readMaybe (unpack mins) :: Maybe Int
        hours   = fmap (\m -> pack . show $ m `div` 60) mins'
        newMins = fmap (\m -> pack . show $ m `mod` 60) mins'
    in  (,) <$> hours <*> newMins
  | otherwise
  = Nothing

lengthFormatted :: PosInt -> Text
lengthFormatted (unrefine -> x) =
  let mins = pack . show $ x `div` 60
      secs = pack . show $ x `mod` 60
      noHour m = maybeAddZero m <> ":" <> maybeAddZero secs
  in  case addHour mins of
        Just (h, m) -> maybeAddZero h <> ":" <> noHour m
        Nothing     -> noHour mins

toAlbumQL :: Album -> AlbumQL
toAlbumQL E.Album {..} = AlbumQL
  (E.unAlbumSpotifyId albumSpotifyId)
  albumName
  albumReleasedYear
  (lengthFormatted $ positive albumTotalLength)
 where
  positive :: Int -> PosInt
  positive = either (\_ -> $$(refineTH 1) :: PosInt) id . refine
