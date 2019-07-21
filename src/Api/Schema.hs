{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

-- | The GraphQL schema
module Api.Schema where

import           Data.Functor                   ( (<&>) )
import           Data.Morpheus.Kind             ( KIND
                                                , OBJECT
                                                )
import           Data.Morpheus.Types            ( GQLType(..)
                                                , ResM
                                                , gqlResolver
                                                )
import           Data.Text
import           Domain                         ( Artist(..)
                                                , ArtistName(..)
                                                )
import           GHC.Generics                   ( Generic )
import           Repository.Artist
import           Utils                          ( maybeToEither )

data Query = Query
  { artist :: ArtistArgs -> ResM ArtistQL
  } deriving Generic

data ArtistQL = ArtistQL
  { fullName :: Text         -- Non-Nullable Field
  , origin   :: Maybe Text   -- Nullable Field
  } deriving (Generic, GQLType)

type instance KIND ArtistQL = OBJECT

data ArtistArgs = ArtistArgs
  { name   :: Text        -- Required Argument
  } deriving Generic

toArtistQL :: Artist -> ArtistQL
toArtistQL a = ArtistQL (artistName a) (Just $ artistOrigin a)

resolveArtist :: ArtistRepository IO -> ArtistArgs -> ResM ArtistQL
resolveArtist repo args = gqlResolver result where
  result = findArtist repo (ArtistName $ name args) <&> \case
    Just a  -> Right $ toArtistQL a
    Nothing -> Left "No hits"

resolveQuery :: ArtistRepository IO -> Query
resolveQuery repo = Query { artist = resolveArtist repo }
