{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Graphics
  ( gamePic
  , countdownPic
  , levelEndPic
  ) where

import Spanout.Common

import Data.Monoid

import qualified Graphics.Gloss as Gloss

import Linear



-- The view of a game state
gamePic :: GameState -> Gloss.Picture
gamePic GameState{..} = Gloss.pictures $
     [ bg
     , ballPic _gsBall
     , batPic _gsBatX
     ]
  ++ map brickPic _gsBricks
  where
    bg = Gloss.color bgColor
       $ Gloss.rectangleSolid screenWidth screenHeight

-- The view of a countdown timer
countdownPic :: RealFrac a => a -> Gloss.Picture
countdownPic t =
    Gloss.scale textScale textScale
  . Gloss.color textColor
  . Gloss.text
  $ show (ceiling t :: Int)

-- A static picture after a level
levelEndPic :: Gloss.Picture
levelEndPic =
    Gloss.color textColor
  . Gloss.scale textScale textScale
  $ Gloss.text "Done"



ballPic :: Ball -> Gloss.Picture
ballPic (Ball {_ballPos = V2 x y}) =
  Gloss.translate x y $ ballProto

batPic :: Float -> Gloss.Picture
batPic x =
  Gloss.translate x batPositionY $ batProto

brickPic :: Brick -> Gloss.Picture
brickPic (Brick (V2 x y) (Circle r)) =
  Gloss.translate x y . Gloss.scale r r $ circleBrickProto
brickPic (Brick (V2 x y) (Rectangle w h)) =
  Gloss.translate x y . Gloss.scale w h $ rectBrickProto

ballProto :: Gloss.Picture
ballProto = Gloss.scale ballRadius ballRadius $ circleFilled ballColor

batProto :: Gloss.Picture
batProto = Gloss.scale batWidth batHeight $ squareFilled batColor

circleBrickProto :: Gloss.Picture
circleBrickProto = circleFilled brickColor

rectBrickProto :: Gloss.Picture
rectBrickProto = squareFilled brickColor

circleFilled :: Gloss.Color -> Gloss.Picture
circleFilled color =
     Gloss.color color (Gloss.circleSolid 1)
  <> Gloss.color (border color) (Gloss.circle 1)

squareFilled :: Gloss.Color -> Gloss.Picture
squareFilled color =
     Gloss.color color (Gloss.polygon squareGeom)
  <> Gloss.color (border color) (Gloss.lineLoop squareGeom)

squareGeom :: Gloss.Path
squareGeom = Gloss.rectanglePath 1 1

-- The color of the border of an object
border :: Gloss.Color -> Gloss.Color
border color = Gloss.rawColor r g b 0.5
  where
    (r, g, b, _) = Gloss.rgbaOfColor $ Gloss.mixColors 0.5 0.5 color bgColor
