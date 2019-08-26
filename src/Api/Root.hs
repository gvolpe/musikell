module Api.Root where

import           Api.Dependencies
import           Api.Schema.Mutation            ( Mutation
                                                , resolveMutation
                                                )
import           Api.Schema.Query               ( Query
                                                , resolveQuery
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( toGraphQLDocument )
import           Data.Morpheus.Types            ( GQLRootResolver(..) )

rootResolver :: Deps -> GQLRootResolver IO () () Query Mutation ()
rootResolver deps = GQLRootResolver
  { queryResolver        = pure $ resolveQuery deps
  , mutationResolver     = pure $ resolveMutation deps
  , subscriptionResolver = return ()
  }

gqlDoc :: Deps -> ByteString
gqlDoc = toGraphQLDocument . Identity . rootResolver

gqlApi :: Deps -> ByteString -> IO ByteString
gqlApi = interpreter . rootResolver
