{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Board (UBoard, BBoard, Box (..), ne, nw, se, sw, toBoxes, fromBoxes) where

import Control.Applicative
import Control.Lens (makeLenses, (^.))
import Data.Array qualified as Boxed
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Array.Unboxed qualified as Unboxed
import Data.Bool
import Data.Function (($))
import Data.Int
import Data.Maybe
import GHC.Num
import GHC.Real (div, even, odd)
import Prelude (Eq, Show)

type UBoard = UArray (Int, Int)

type BBoard = Array (Int, Int)

data Box = Box
  { _ne :: !Bool,
    _nw :: !Bool,
    _se :: !Bool,
    _sw :: !Bool
  }
  deriving (Eq, Show)

makeLenses ''Box

toBoxes :: Bool -> UBoard Bool -> Maybe (BBoard Box)
toBoxes doShift board =
  if odd w && odd h -- because w (h) is one less than the width (hight) of the board
    then Just $ Boxed.array newBounds list
    else Nothing
  where
    newBounds = ((xStart, yStart), (xStart + w, yStart + h))
    ((xStart, yStart), (xEnd, yEnd)) = bounds board
    w = (xEnd - xStart) `div` 2
    h = (yEnd - yStart) `div` 2
    list = do
      (x, y) <- Unboxed.range newBounds
      let (x', y') = if doShift then (x * 2 + 1, y * 2 + 1) else (x * 2, y * 2)
          ne' = board ! (x', y')
          nw' = board ! (x' - 1, y')
          se' = board ! (x', y' - 1)
          sw' = board ! (x' - 1, y' - 1)
      pure ((x, y), Box ne' nw' se' sw')

fromBoxes :: Bool -> BBoard Box -> UBoard Bool
fromBoxes doShift boxes =
  Unboxed.array newBounds list
  where
    newBounds = ((xStart, yStart), (xStart + w, yStart + h))
    w = (xEnd - xStart) * 2
    h = (yEnd - yStart) * 2
    ((xStart, yStart), (xEnd, yEnd)) = bounds boxes
    list = do
      (x, y) <- Boxed.range newBounds
      let (x', y') = if doShift then ((x - 1) `div` 2, (y - 1) `div` 2) else (x `div` 2, y `div` 2)
          box = boxes ! (x', y')
          lens =
            case (even x, even y) of
              (True, True) -> ne
              (False, True) -> nw
              (True, False) -> se
              (False, False) -> sw
      pure ((x, y), box ^. lens)
