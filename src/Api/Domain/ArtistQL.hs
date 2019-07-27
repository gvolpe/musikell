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
  { spotifyId :: Maybe Text   -- Nullable Field
  , name :: Text              -- Non-Nullable Field
  } deriving (Generic, GQLType)

type instance KIND ArtistQL = OBJECT

toArtistQL :: Artist -> ArtistQL
toArtistQL a = ArtistQL (Just $ E.artistSpotifyId a) (E.artistName a)
