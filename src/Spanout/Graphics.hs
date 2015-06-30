{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Graphics
  ( gamePic
  , countdownPic
  ) where

import Spanout.Common

import Data.Monoid ((<>))

import qualified Graphics.Gloss as Gloss

import Linear



gamePic :: GameState -> Gloss.Picture
gamePic GameState{..} = Gloss.pictures $
     [ bg ]
  ++ map brickPic _gsBricks
  ++ [ batPic _gsBatX
     , ballPic _gsBall
     ]
  where
    bg = Gloss.color bgColor
       $ Gloss.rectangleSolid screenWidth screenHeight

countdownPic :: RealFrac a => a -> Gloss.Picture
countdownPic t =
    Gloss.scale countdownTextScale countdownTextScale
  . Gloss.color countdownTextColor
  . Gloss.text
  $ show (ceiling t :: Int)



ballPic :: Ball -> Gloss.Picture
ballPic (Ball {_ballPos = V2 x y}) =
  Gloss.translate x y $ circleFilled ballColor ballRadius

batPic :: Float -> Gloss.Picture
batPic x =
  Gloss.translate x batPositionY $ rectangleFilled batColor batWidth batHeight

brickPic :: Brick -> Gloss.Picture
brickPic (Brick (V2 x y) (Circle r)) =
  Gloss.translate x y $ circleFilled brickColor r
brickPic (Brick (V2 x y) (Rectangle w h)) =
  Gloss.translate x y $ rectangleFilled brickColor w h

circleFilled :: Gloss.Color -> Float -> Gloss.Picture
circleFilled color radius =
     Gloss.color color (Gloss.circleSolid radius)
  <> Gloss.color (Gloss.dark color) (Gloss.circle radius)

rectangleFilled :: Gloss.Color -> Float -> Float -> Gloss.Picture
rectangleFilled color width height =
     Gloss.color color (Gloss.rectangleSolid width height)
  <> Gloss.color (Gloss.dark color) (Gloss.rectangleWire width height)
