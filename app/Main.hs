{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config                         ( loadConfig )
import           Http.Server                    ( serve )

main :: IO ()
main = loadConfig >>= serve
