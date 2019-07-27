-- | The Neo4j connection pool.
module Repository.Neo where

import           Config                         ( Neo4jConfig(..) )
import           Data.Default                   ( def )
import           Data.Pool                      ( Pool
                                                , createPool
                                                )
import           Database.Bolt

mkPipePool :: Neo4jConfig -> IO (Pool Pipe)
mkPipePool c = createPool acquire release 1 3600 10 where
  acquire = connect $ def { user = neo4jUser c, password = neo4jPassword c }
  release = close
