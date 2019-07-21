module Api.Root where

import           Api.Schema                     ( Query
                                                , resolveQuery
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Types            ( GQLRootResolver(..) )
import           Repository.Artist

rootResolver :: ArtistRepository IO -> GQLRootResolver IO Query () ()
rootResolver repo = GQLRootResolver
  { queryResolver        = return $ resolveQuery repo
  , mutationResolver     = return ()
  , subscriptionResolver = return ()
  }

gqlApi :: ArtistRepository IO -> ByteString -> IO ByteString
gqlApi repo = interpreter $ rootResolver repo
