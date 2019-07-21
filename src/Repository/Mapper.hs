{-# LANGUAGE OverloadedStrings #-}

-- | The Neo4j Mapper type class, its instances and some helper functions.
module Repository.Mapper where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text
import           Database.Bolt
import           Domain

type NodeProps = Map Text Value

class NodeMapper a where
  toEntity :: NodeProps -> Maybe a

instance NodeMapper Artist where
  toEntity p =
    Artist
      <$> (Map.lookup "name" p >>= exact :: Maybe Text)
      <*> (Map.lookup "origin" p >>= exact :: Maybe Text)

toNode :: Monad m => Record -> m Node
toNode record = record `at` "a" >>= exact

toNodeProps :: Monad m => Record -> m NodeProps
toNodeProps r = nodeProps <$> toNode r
