module Api.Root where

import           Api.Dependencies
import           Api.Schema                     ( Mutation
                                                , Query
                                                , resolveMutation
                                                , resolveQuery
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Types            ( GQLRootResolver(..) )

rootResolver :: Deps -> GQLRootResolver IO Query Mutation ()
rootResolver deps = GQLRootResolver
  { queryResolver        = pure $ resolveQuery (albumRepository deps)
                                               (artistRepository deps)
  , mutationResolver     = pure $ resolveMutation deps
  , subscriptionResolver = return ()
  }

gqlApi :: Deps -> ByteString -> IO ByteString
gqlApi deps = interpreter $ rootResolver deps
