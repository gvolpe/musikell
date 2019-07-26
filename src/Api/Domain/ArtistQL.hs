{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies #-}

module Api.Domain.ArtistQL where

import           Data.Morpheus.Kind             ( KIND
                                                , OBJECT
                                                )
import           Data.Morpheus.Types            ( GQLType )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Repository.Entity              ( Artist )
import qualified Repository.Entity             as E

data ArtistQL = ArtistQL
  { name :: Text              -- Non-Nullable Field
  , spotifyId :: Maybe Text   -- Nullable Field
  } deriving (Generic, GQLType)

type instance KIND ArtistQL = OBJECT

toArtistQL :: Artist -> ArtistQL
toArtistQL a = ArtistQL (E.artistName a) (Just $ E.artistSpotifyId a)
