module Spanout.Level
  ( generateBricks

  , test
  ) where

import Spanout.Common

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Random

import Data.Fixed (divMod')
import Data.List (sort)

import Linear

import qualified Test.Framework as Test
import qualified Test.Framework.Providers.QuickCheck2 as Test



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
  relLevelOffsetY <- pure 0 --getRandomR (0.05, (1 - relLevelHeight) - 0.05)
  let
    levelHeight = scrHeight * relLevelHeight
    offsetY = -(relLevelOffsetY / 2) * levelHeight
    rowBottoms = map (subtract levelHeight) $ scanl (+) 0 rowHeights
    rowYs = alignRows rowHeights
    placedRows = zipWith placeRow rowYs rows
  return (concat placedRows, LevelGeom levelHeight offsetY rowYs)
  where
    placeRow y = over (mapped . brPos . _y) (+y)
    scrWidth = fromIntegral screenWidth
    scrHeight = fromIntegral screenHeight
    avg a b = (a + b) / 2



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

alignRows :: [Float] -> [Float]
alignRows heights = heights

brickWidth :: Float
brickWidth = 80

brickHeight :: Float
brickHeight = 30



test :: Test.Test
test = Test.testGroup "Spanout.Level"
  [ Test.testProperty
      "alignRows results are ascending"
      alignRows_resultAscending
  ]

alignRows_resultAscending :: [Float] -> Bool
alignRows_resultAscending heights = centers == sort centers
  where
    centers = alignRows heights
