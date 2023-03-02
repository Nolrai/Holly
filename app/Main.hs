{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Monad
-- import Data.Either
-- import Data.String
import Graphics.Gloss
-- import System.Environment
import System.IO

main :: IO ()
main = do
  display (InWindow "Nice Window" (200, 200) (10, 10)) blue (Circle 80)
