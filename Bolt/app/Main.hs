module Main where

import Lib

main = do
  file <- readFile "/home/dinnu93/Desktop/test.json~"
  putStrLn . show . encodeJSON $ file
