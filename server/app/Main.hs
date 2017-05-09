module Main where

import           Server

main :: IO ()
main = startApp "sqlite.db"
