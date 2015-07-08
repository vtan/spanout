module Spanout.Level (generateBricks) where

import Spanout.Common

import Control.Lens
import Control.Monad
import Control.Monad.Random

import Data.Fixed

import Linear



-- Generates bricks in randomly sized rows
generateBricks :: MonadRandom m => m [Brick]
generateBricks = do
  relLevelHeight <- getRandomR (0.3, 0.6)
  relRowHeights <- splitRow relLevelHeight
  let rowHeights = map (screenHeight *) relRowHeights
  rows <- forM rowHeights $ \h -> do
    relW <- getRandomR (0.2, 0.8)
    shape <- getRandom
    let
      w = screenWidth * relW
      gen
        | shape < (0.3 :: Float) = fillCircles
        | otherwise              = fillRectangles
    return $ gen w h
  let levelHeight = screenHeight * relLevelHeight
  offset <- getRandomR (0, screenBoundY - levelHeight / 2 - 2 * ballRadius)
  let
    rowYs = alignRows offset rowHeights
    placedRows = zipWith placeRow rowYs rows
  case concat placedRows of
    []     -> generateBricks
    bricks -> return bricks
  where
    placeRow y = over (mapped . brPos . _y) (+y)



-- Splits a row of a given height randomly
splitRow :: MonadRandom m => Float -> m [Float]
splitRow h
  | h <= 0.15 = return [h]
  | otherwise = do
      splitFurther <- getRandom
      if splitFurther < (0.8 :: Float)
      then do
        ratio <- getRandomR (0.3, 0.7)
        liftM2 (++) (splitRow (ratio * h)) (splitRow ((1 - ratio) * h))
      else return [h]

-- Fills the rectangle centered at the origin with bricks
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

-- Fills the rectangle centered at the origin with bricks
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
-- resulting list of rows is centered at the origin
alignRows :: Float -> [Float] -> [Float]
alignRows offset heights =
  map (offset+) $ zipWith avg (init alignedBottoms) (tail alignedBottoms)
  where
    bottoms = scanl (+) 0 heights
    alignedBottoms = map (subtract $ sum heights / 2) bottoms
    avg x y = (x + y) / 2
