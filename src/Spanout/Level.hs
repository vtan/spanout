module Spanout.Level (generateBricks) where

import Spanout.Common

import Control.Monad.Random

import Data.Fixed (divMod')

import Linear



generateBricks :: MonadRandom m => m [Brick]
generateBricks = do
  relW <- getRandomR (0.5, 0.9)
  relH <- getRandomR (0.2, 0.6)
  let
    w = relW * fromIntegral screenWidth
    h = relH * fromIntegral screenHeight
  return $ generateBricksInRect w h



generateBricksInRect :: Float -> Float -> [Brick]
generateBricksInRect w h =
  map rectBrick [(x, y) | x <- [0 .. countX - 1], y <- [0 .. countY - 1]]
  where
    (countX, marginX) = w `divMod'` brickWidth
    (countY, marginY) = h `divMod'` brickHeight
    startX = negate $ (w - marginX) / 2
    startY = negate $ (h - marginY) / 2

    rectBrick :: (Int, Int) -> Brick
    rectBrick (x, y) = Rectangle (V2 px py) brickWidth brickHeight
      where
        px = startX + fromIntegral x * brickWidth
        py = startY + fromIntegral y * brickHeight

brickWidth :: Float
brickWidth = 80

brickHeight :: Float
brickHeight = 30
