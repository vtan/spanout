{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Graphics
  ( gamePic
  , countdownPic
  , levelEndPic
  ) where

import Spanout.Common

import Data.Monoid ((<>))

import qualified Graphics.Gloss as Gloss

import Linear



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

countdownPic :: RealFrac a => a -> Gloss.Picture
countdownPic t =
    Gloss.scale textScale textScale
  . Gloss.color textColor
  . Gloss.text
  $ show (ceiling t :: Int)

levelEndPic :: Gloss.Picture
levelEndPic =
    Gloss.color textColor
  . Gloss.scale textScale textScale
  $ Gloss.text "Done"



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
