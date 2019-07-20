module Main where

import Neo

main :: IO ()
main = do
  artistRepo <- mkArtistRepository
  foo artistRepo
