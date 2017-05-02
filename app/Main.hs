module Main where

import Parser

main :: IO ()
main = do
  contents <- getContents
  putStr contents
