module Main (main) where

import Data.String.Strip

main :: IO ()
main = do
  let s = "  Hello, World!  "
  putStrLn $ strip s
