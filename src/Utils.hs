module Utils where

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (x : _) = Just x
