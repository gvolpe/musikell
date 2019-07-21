-- | A bunch of common helper functions
module Utils where

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (x : _) = Just x
