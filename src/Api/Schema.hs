{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

-- | The GraphQL schema
module Api.Schema where

import           Api.Args.Artist                ( ArtistArgs )
import qualified Api.Args.Artist               as Args
import           Data.Functor                   ( (<&>) )
import           Data.Morpheus.Kind             ( KIND
                                                , OBJECT
                                                )
import           Data.Morpheus.Types            ( GQLType(..)
                                                , ResM
                                                , gqlResolver
                                                )
import           Data.Text
import           GHC.Generics                   ( Generic )
import           Repository.Artist
import           Repository.Entity              ( Artist
                                                , ArtistName(..)
                                                )
import qualified Repository.Entity             as E
import           Utils                          ( maybeToEither )

data Query = Query
  { artist :: ArtistArgs -> ResM ArtistQL
  } deriving Generic

data ArtistQL = ArtistQL
  { name :: Text              -- Non-Nullable Field
  , spotifyId :: Maybe Text   -- Nullable Field
  } deriving (Generic, GQLType)

type instance KIND ArtistQL = OBJECT

toArtistQL :: Artist -> ArtistQL
toArtistQL a = ArtistQL (E.artistName a) (Just $ E.artistSpotifyId a)

resolveArtist :: ArtistRepository IO -> ArtistArgs -> ResM ArtistQL
resolveArtist repo args = gqlResolver result where
  result = findArtist repo (ArtistName $ Args.name args) <&> \case
    Just a  -> Right $ toArtistQL a
    Nothing -> Left "No hits"

resolveQuery :: ArtistRepository IO -> Query
resolveQuery repo = Query { artist = resolveArtist repo }
