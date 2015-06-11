module Spanout.Level
  ( generateBricks

  , test
  ) where

import Spanout.Common

import Control.Applicative
import Control.Lens (over, mapped)
import Control.Monad
import Control.Monad.Random

import Data.AEq (AEq, (~==))
import Data.Fixed (divMod')
import Data.List (sort)

import Linear

import qualified Test.Framework as Test
import qualified Test.Framework.Providers.QuickCheck2 as Test
import qualified Test.QuickCheck as Test



generateBricks :: (MonadRandom m, Applicative m) => m ([Brick], LevelGeom)
generateBricks = do
  relLevelHeight <- getRandomR (0.2, 0.6)
  relRowHeights <- splitRow relLevelHeight
  let rowHeights = map (scrHeight *) relRowHeights
  rows <- forM rowHeights $ \h -> do
    relW <- getRandomR (0.3, 0.9)
    shape <- getRandom
    let
      w = scrWidth * relW
      gen
        | shape < (0.5 :: Float) = fillCircles
        | otherwise              = fillRectangles
    return $ gen w h
  let levelHeight = scrHeight * relLevelHeight
  offset <- getRandomR (0, screenBoundY - levelHeight / 2 - 2 * ballRadius)
  let
    rowYs = alignRows offset rowHeights
    placedRows = zipWith placeRow rowYs rows
  return (concat placedRows, LevelGeom levelHeight offset rowYs)
  where
    placeRow y = over (mapped . brPos . _y) (+y)
    scrWidth = screenWidth
    scrHeight = screenHeight



splitRow :: (MonadRandom m, Applicative m) => Float -> m [Float]
splitRow h
  | h <= 0.1 = return [h]
  | otherwise = do
      shouldSplit <- getRandom
      if shouldSplit < (0.6 :: Float)
      then do
        ratio <- getRandomR (0.2, 0.8)
        (++) <$> splitRow (ratio * h) <*> splitRow ((1 - ratio) * h)
      else return [h]

-- Fills the rectangle centered at the origin with bricks.
fillCircles :: Float -> Float -> [Brick]
fillCircles w h
  | h >= brickHeight = map circBrick [0 .. countX - 1]
  | otherwise        = []
  where
    r = h / 2
    (countX, marginX) = w `divMod'` h
    startX = negate $ (w - marginX - h) / 2
    y = 0
    circBrick :: Int -> Brick
    circBrick x = Brick (V2 (startX + fromIntegral x * h) y) $ Circle r

-- Fills the rectangle centered at the origin with bricks.
fillRectangles :: Float -> Float -> [Brick]
fillRectangles w h =
  map rectBrick [V2 x y | x <- [0 .. countX - 1], y <- [0 .. countY - 1]]
  where
    (countX, marginX) = w `divMod'` brickWidth
    (countY, marginY) = h `divMod'` brickHeight
    startX = negate $ (w - marginX - brickWidth) / 2
    startY = negate $ (h - marginY - brickHeight) / 2

    rectBrick :: V2 Int -> Brick
    rectBrick (V2 x y) = Brick (V2 px py) $ Rectangle brickWidth brickHeight
      where
        px = startX + fromIntegral x * brickWidth
        py = startY + fromIntegral y * brickHeight

-- Calculates the vertical row centers based on the row heights, so the
-- resulting list of rows is centered at the origin.
alignRows :: Float -> [Float] -> [Float]
alignRows offset heights =
  map (offset+) $ zipWith avg (init alignedBottoms) (tail alignedBottoms)
  where
    bottoms = scanl (+) 0 heights
    alignedBottoms = map (subtract $ sum heights / 2) bottoms
    avg x y = (x + y) / 2

brickWidth :: Float
brickWidth = 0.3

brickHeight :: Float
brickHeight = 0.1



test :: Test.Test
test = Test.testGroup "Spanout.Level"
  [ Test.testProperty
     "alignRows results are bounded by the sum of row heights"
      alignRows_allInsideBounds
  , Test.testProperty
     "alignRows results touch bottom and top of bounding rectangle"
      alignRows_touchesBounds
  , Test.testProperty
     "alignRows results are ascending"
      alignRows_ascending
  ]

alignRows_allInsideBounds :: Float -> [Test.Positive Float] -> Bool
alignRows_allInsideBounds offset heights' =
  all inside $ zip heights centers
  where
    heights = map Test.getPositive heights'
    centers = alignRows offset heights
    inside (height, center) = center - height / 2 ~>= offset - bound
                           && center + height / 2 ~<= offset + bound
    bound = sum heights / 2

alignRows_touchesBounds ::
  Float -> Test.NonEmptyList (Test.Positive Float) -> Bool
alignRows_touchesBounds offset heights' =
  touchesBottom && touchesTop
  where
    heights = map Test.getPositive $ Test.getNonEmpty heights'
    centers = alignRows offset heights
    touchesBottom = firstCenter - firstHeight / 2 ~== offset - bound
    touchesTop    = lastCenter  + lastHeight  / 2 ~== offset + bound
    (firstHeight, firstCenter) = (head heights, head centers)
    (lastHeight,  lastCenter)  = (last heights, last centers)
    bound = sum heights / 2

alignRows_ascending :: Float -> [Test.Positive Float] -> Bool
alignRows_ascending offset heights' = centers == sort centers
  where
    heights = map Test.getPositive heights'
    centers = alignRows offset heights

infix 4 ~>=
(~>=) :: (Ord a, AEq a) => a -> a -> Bool
x ~>= y = x >= y || x ~== y

infix 4 ~<=
(~<=) :: (Ord a, AEq a) => a -> a -> Bool
x ~<= y = x <= y || x ~== y
