{-# LANGUAGE DeriveGeneric, TypeFamilies #-}

module Api.Domain.ArtistQL where

import           Data.Morpheus.Kind             ( OBJECT )
import           Data.Morpheus.Types            ( GQLType(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Repository.Entity              ( Artist )
import qualified Repository.Entity             as E

data ArtistQL = ArtistQL
  { spotifyId :: Text
  , name :: Text
  } deriving Generic

instance GQLType ArtistQL where
  type KIND ArtistQL = OBJECT

toArtistQL :: Artist -> ArtistQL
toArtistQL a =
  ArtistQL (E.unArtistSpotifyId (E.artistSpotifyId a)) (E.artistName a)
