{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.BoardSpec (main, spec) where

import Control.Applicative (pure, (<*>))
import Control.Monad ((=<<))
import Data.Array.IArray (IArray, listArray, rangeSize)
import Data.Board
import Data.Bool
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import GHC.Int (Int)
import GHC.Num ((*), (+), (-))
import GHC.Real (mod)
import System.IO (IO)
import Test.Hspec
import Test.QuickCheck
import Prelude (Show (..))

-- import Data.Monoid ((<>))
-- import Debug.Trace (trace)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

instance Arbitrary Box where
  arbitrary = Box <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, IArray arr a, Show a) => Arbitrary (arr (Int, Int) a) where
  arbitrary = do
    (xStart :: Int) <- arbitrary
    (yStart :: Int) <- arbitrary
    (size :: Int) <- (+ 1) <$> getSize
    (width :: Int) <- (+ 1) . (`mod` size) <$> arbitrary
    (hight :: Int) <- (+ 1) . (`mod` size) <$> arbitrary
    let (bounds' :: ((Int, Int), (Int, Int))) = ((xStart, yStart), (xStart + width, yStart + hight))
    list <- vectorOf (rangeSize bounds') arbitrary
    pure $ listArray bounds' list

spec :: Spec
spec = do
  describe "Boxes" $ do
    it "has an Arbitrary instance" $
      property $
        \box -> box `shouldBe` (box :: Box)
  describe "UBoard" $ do
    it "has an Arbitrary instance" $
      property $
        \board -> board `shouldBe` (board :: UBoard Bool)
  describe "BBoard" $ do
    it "has an Arbitrary instance" $
      property $
        \board -> board `shouldBe` (board :: BBoard Bool)
  describe "toBoxes" $ do
    it "is the inverse of fromBoxes" $
      property $
        \(doShift :: Bool) (boxes :: BBoard Box) ->
          let board = fromBoxes doShift boxes
              newBoxes = toBoxes doShift board
           in (newBoxes, board) `shouldBe` (Just boxes, board)
