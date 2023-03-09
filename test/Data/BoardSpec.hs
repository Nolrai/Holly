{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.BoardSpec {-(main, spec)-} where

import Control.Applicative (pure, (<*>))
import Control.Lens ((^.))
import Control.Monad (Monad, mapM_, when, (=<<))
import Data.Array.IArray (Array, IArray, Ix, array, bounds, elems, listArray, rangeSize)
import Data.Array.Unboxed (UArray)
import Data.Board
import Data.Bool
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List qualified as List
import Data.Maybe (Maybe (..), isJust)
import Data.Monoid ((<>))
import GHC.Int (Int)
import GHC.Num ((*), (+), (-))
import GHC.Real (div, mod, (^))
import System.IO (IO)
import Test.Hspec
import Test.Hspec.SmallCheck as SC
import Test.QuickCheck as QC
import Test.SmallCheck as SC
import Test.SmallCheck.Series qualified as SC
import Prelude (Bounded, Eq, Ord (..), Show (..), maxBound, minBound)

-- import Data.Monoid ((<>))
-- import Debug.Trace (trace)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

instance (Monad m, SC.Serial m Bool) => SC.Serial m Box

instance Arbitrary Box where
  arbitrary = Box <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = shrinkBox

shrinkBox :: Box -> [Box]
shrinkBox box = List.delete box $ shrinkBox'
  where
    shrinkBox' = Box <$> shrink' (box ^. ne) <*> shrink' (box ^. nw) <*> shrink' (box ^. se) <*> shrink' (box ^. sw)
    shrink' :: Bool -> [Bool]
    shrink' True = [False, True]
    shrink' False = [False]

shrinkBoxSpec box = List.filter (`paretoLessThan` box) (SC.listSeries maxBound)

paretoLessThan (Box ne nw se sw) (Box ne' nw' se' sw') =
  (ne <= ne' && nw <= nw' && se <= se' && sw <= sw') && (ne < ne' || nw < nw' || se < se' || sw < sw')

instance (Bounded a, Arbitrary a, IArray UArray a, Show a, Eq a) => Arbitrary (UArray (Int, Int) a) where
  arbitrary = arbitrary'
  shrink = shrink'

instance (Bounded a, Arbitrary a, IArray Array a, Show a, Eq a) => Arbitrary (Array (Int, Int) a) where
  arbitrary = arbitrary'
  shrink = shrink'

arbitrary' :: (Arbitrary a, IArray arr a) => Gen (arr (Int, Int) a)
arbitrary' = do
  (xStart :: Int) <- arbitrary
  (yStart :: Int) <- arbitrary
  (size :: Int) <- (+ 1) <$> getSize
  (width :: Int) <- (+ 1) . (`mod` size) <$> arbitrary
  (hight :: Int) <- (+ 1) . (`mod` size) <$> arbitrary
  let (bounds' :: ((Int, Int), (Int, Int))) = ((xStart, yStart), (xStart + width, yStart + hight))
  list <- vectorOf (rangeSize bounds') arbitrary
  pure $ listArray bounds' list

shrink' :: forall a arr. (Bounded a, Arbitrary a, IArray arr a, Eq (arr (Int, Int) a)) => arr (Int, Int) a -> [arr (Int, Int) a]
shrink' arr = shrunkArrays
  where
    -- shrunkArrays are arrays that are smaller than the original array
    shrunkArrays :: [arr (Int, Int) a]
    shrunkArrays =
      List.filter
        (\newArr -> rangeSize (bounds newArr) < arrSize)
        [listArray newBounds (elems arr) | newBounds <- shrinkBounds (bounds arr)]
    arrSize = rangeSize (bounds arr)
    shrinkBounds ((xStart, yStart), (xEnd, yEnd)) =
      fromStartAndSizxe <$> shrinkPair (xStart, yStart) <*> shrinkPair (xEnd - xStart, yEnd - yStart)
    shrinkPair (x, y) = (,) <$> [x, x `div` 2] <*> [y, y `div` 2]

fromStartAndSizxe :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
fromStartAndSizxe (xStart, yStart) (width, hight) =
  ((xStart, yStart), (xStart + max (width - 1) 0, yStart + max (hight - 1) 0))

total' :: (Eq a, Show a) => a -> Expectation
total' x = x `shouldBe` x

shrinkLoop :: (Arbitrary a) => Int -> a -> Expectation
shrinkLoop 0 _ = expectationFailure "Ran out of fuel"
shrinkLoop depth x = mapM_ (shrinkLoop (depth - 1)) (shrink x)

spec :: Spec
spec = do
  describe "Boxes Arbitray instance" $ do
    it "has implements arbitrary instance" $
      QC.property $
        \box -> total' (box :: Box)
    it "has a shrink that matches shrinkBoxSpec" $
      SC.property $
        \box -> shrink (box :: Box) `shouldMatchList` shrinkBoxSpec box
    it "has a shrink that terminates" $
      SC.property $
        \box -> shrinkLoop 20 (box :: Box)
  describe "UBoard" $ do
    it "has an Arbitrary instance" $
      QC.property $
        \board -> total' (board :: UBoard Bool)
    it "has a shrink that terminates" $
      QC.property $
        \board -> do
          size <- getSize
          pure (shrinkLoop (max size 5) (board :: Box))
  describe "BBoard" $ do
    it "has an Arbitrary instance" $
      QC.property $
        \board -> total' (board :: BBoard Bool)
  describe "fromBoxes" $ do
    it "doesnt crash (example 0)" $
      fromBoxesDoesntCrash True failingExample0
    it "doesnt crash" $
      QC.property fromBoxesDoesntCrash
    it "is the (partial) inverse of toBoxes" $
      QC.property fromBoxesIsTheInverseOfToBoxes
  describe "toBoxes" $ do
    it "doesnt crash" $
      QC.property $ \(doShift :: Bool) (board :: UBoard Bool) ->
        let boxes = toBoxes doShift board
         in boxes `shouldBe` boxes
    it "is the inverse of fromBoxes (example 1)" $
      toBoxesIsTheInverseOfFromBoxes False failingExample1
    it "is the inverse of fromBoxes" $
      QC.property fromBoxesIsTheInverseOfToBoxes

fromBoxesDoesntCrash :: Bool -> BBoard Box -> Expectation
fromBoxesDoesntCrash (doShift :: Bool) (boxes :: BBoard Box) =
  board `shouldBe` board
  where
    board = fromBoxes doShift boxes

fromBoxesIsTheInverseOfToBoxes :: Bool -> UBoard Bool -> Expectation
fromBoxesIsTheInverseOfToBoxes (doShift :: Bool) (board :: UBoard Bool) =
  let boxes = toBoxes doShift board
      newBoard = fromBoxes doShift <$> boxes
   in when
        (isJust newBoard)
        (cmpr (boxes, newBoard) `shouldBe` cmpr (boxes, Just board))

toBoxesIsTheInverseOfFromBoxes :: Bool -> BBoard Box -> Expectation
toBoxesIsTheInverseOfFromBoxes (doShift :: Bool) (boxes :: BBoard Box) =
  let board = fromBoxes doShift boxes
      newBoxes = toBoxes doShift board
   in (board, newBoxes) `shouldBe` (board, Just boxes)

-- used in "it doesnt crash" tests
failingExample0 :: BBoard Box
failingExample0 =
  array ((0, 0), (1, 1)) list
  where
    list =
      [ ((0, 0), Box {_ne = True, _nw = False, _se = False, _sw = False}),
        ((0, 1), Box {_ne = False, _nw = False, _se = True, _sw = False}),
        ((1, 0), Box {_ne = True, _nw = True, _se = False, _sw = False}),
        ((1, 1), Box {_ne = False, _nw = True, _se = True, _sw = True})
      ]

-- used in "toBoxes is the inverse of fromBoxes" test
failingExample1 :: BBoard Box
failingExample1 =
  array ((0, 0), (1, 1)) list
  where
    list =
      [ ((0, 0), Box {_ne = False, _nw = False, _se = True, _sw = True}),
        ((0, 1), Box {_ne = True, _nw = True, _se = False, _sw = False}),
        ((1, 0), Box {_ne = True, _nw = True, _se = False, _sw = False}),
        ((1, 1), Box {_ne = True, _nw = False, _se = False, _sw = False})
      ]
