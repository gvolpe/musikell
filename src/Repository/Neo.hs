{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j connection pool.
module Repository.Neo
  ( mkPipePool
  )
where

import           Config                         ( Neo4jConfig(..) )
import           Data.Pool                      ( Pool
                                                , createPool
                                                )
import           Data.Text                      ( unpack )
import           Database.Bolt           hiding ( unpack )
import           GHC.Natural                    ( naturalToInt )

mkConfig :: Neo4jConfig -> BoltCfg
mkConfig c = BoltCfg { magic         = 1616949271
                     , version       = 1
                     , userAgent     = "hasbolt/1.3"
                     , maxChunkSize  = 65535
                     , socketTimeout = 5
                     , host          = unpack $ neo4jHost c
                     , port          = naturalToInt $ neo4jPort c
                     , user          = neo4jUser c
                     , password      = neo4jPassword c
                     , secure        = neo4jSecure c
                     }

mkPipePool :: Neo4jConfig -> IO (Pool Pipe)
mkPipePool c = createPool acquire release 1 3600 10 where
  acquire = connect (mkConfig c)
  release = close
