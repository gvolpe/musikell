module Api.Root where

import           Api.Dependencies
import           Api.Schema                     ( Query
                                                , resolveQuery
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Types            ( GQLRootResolver(..) )

rootResolver :: Deps -> GQLRootResolver IO Query () ()
rootResolver deps = GQLRootResolver
  { queryResolver        = return $ resolveQuery (albumRepository deps)
                                                 (artistRepository deps)
  , mutationResolver     = return () -- TODO: mutation that calls loadData
  , subscriptionResolver = return ()
  }

gqlApi :: Deps -> ByteString -> IO ByteString
gqlApi deps = interpreter $ rootResolver deps
