{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Board (UBoard, BBoard, Box (..), ne, nw, se, sw, toBoxes, fromBoxes, cmpr) where

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
import GHC.Generics
import GHC.Num
import GHC.Real (div, even, mod, odd)
import Prelude

type UBoard = UArray (Int, Int)

type BBoard = Array (Int, Int)

data Box = Box
  { _ne :: !Bool,
    _nw :: !Bool,
    _se :: !Bool,
    _sw :: !Bool
  }
  deriving (Eq, Ord, Generic, Bounded)

makeLenses ''Box

instance Show Box where
  show (Box ne nw se sw) = "(" <> map toBit [ne, nw, se, sw] <> ")"
    where
      toBit True = '1'
      toBit False = '0'

-- mirror the array into a torus.
(!^) :: IArray arr a => arr (Int, Int) a -> (Int, Int) -> a
arr !^ (x, y) = arr ! (x', y')
  where
    ((xStart, yStart), (xEnd, yEnd)) = bounds arr
    x' = xStart + (x `mod` (xEnd - xStart))
    y' = yStart + (y `mod` (yEnd - yStart))

class CompressedShow a where
  compressedShow :: a -> String

instance CompressedShow Bool where
  compressedShow True = "1"
  compressedShow False = "0"

instance CompressedShow Box where
  compressedShow = show

instance (Ix i, CompressedShow e) => CompressedShow (Array i e) where
  compressedShow arr = concatMap compressedShow (elems arr)

instance (Ix i, IArray UArray e, CompressedShow e) => CompressedShow (UArray i e) where
  compressedShow arr = concatMap compressedShow (elems arr)

instance CompressedShow e => CompressedShow (Maybe e) where
  compressedShow (Just e) = "[" <> compressedShow e <> "]"
  compressedShow Nothing = "[]"

instance (CompressedShow e, CompressedShow b) => CompressedShow (e, b) where
  compressedShow (x, y) = compressedShow x <> " " <> compressedShow y

cmpr :: CompressedShow a => a -> String
cmpr = compressedShow

toBoxes :: Bool -> UBoard Bool -> Maybe (BBoard Box)
toBoxes doShift board =
  if even (getWidth board) && even (getHight board)
    then Just $ Boxed.array newBounds list
    else Nothing
  where
    newBounds = ((xStart, yStart), (xStart + w, yStart + h))
    ((xStart, yStart), (xEnd, yEnd)) = bounds board
    w = getWidth `div` 2
    h = getHight `div` 2
    list = do
      (x, y) <- Unboxed.range newBounds
      let (x', y') = if doShift then (x * 2 + 1, y * 2 + 1) else (x * 2, y * 2)
          ne' = board !^ (x', y')
          nw' = board !^ (x' - 1, y')
          se' = board !^ (x', y' - 1)
          sw' = board !^ (x' - 1, y' - 1)
      pure ((x, y), Box ne' nw' se' sw')

getWidth :: IArray arr a => arr (Int, Int) a -> Int
getWidth arr = xEnd - xStart + 1
  where
    ((xStart, _), (xEnd, _)) = bounds arr

getHight :: IArray arr a => arr (Int, Int) a -> Int
getHight arr = yEnd - yStart + 1
  where
    ((_, yStart), (_, yEnd)) = bounds arr

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
          box = boxes !^ (x', y')
          lens =
            case (even x, even y) of
              (True, True) -> ne
              (False, True) -> nw
              (True, False) -> se
              (False, False) -> sw
      pure ((x, y), box ^. lens)
