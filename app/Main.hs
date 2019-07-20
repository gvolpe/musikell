module Main where

import           Repository.Artist
import           Repository.Neo                 ( mkPipePool
                                                , showArtist
                                                )

main :: IO ()
main = do
  pool       <- mkPipePool
  artistRepo <- mkArtistRepository pool
  showArtist artistRepo
