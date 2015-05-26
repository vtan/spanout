{-# LANGUAGE TypeOperators #-}

module Spanout.Graphics
  ( gameDisplay
  , countdownDisplay
  ) where

import Spanout.Common

import Control.Arrow
import Control.Lens

import Data.Monoid ((<>))

import qualified Graphics.Gloss as Gloss

import Linear



gameDisplay :: GameState ->> Gloss.Picture
gameDisplay = arr gamePic

countdownDisplay :: (GameState, Float) ->> Gloss.Picture
countdownDisplay = arr $ uncurry countdownPic



gamePic :: GameState -> Gloss.Picture
gamePic gs = Gloss.pictures $
     [ levelGeomPic $ view gsLevelGeom gs ]
  ++ map brickPic (view gsBricks gs)
  ++ [ ballPic      $ view (gsBall . ballPos) gs
     , batPic       $ view gsBatX gs
     , lastCollPic  $ view gsLastCollision gs
     ]

countdownPic :: GameState -> Float -> Gloss.Picture
countdownPic gs remaining = gamePic gs <> remainingText
  where
    remainingText = Gloss.color Gloss.chartreuse $ Gloss.text str
    str = show (ceiling remaining :: Int)

ballPic :: V2 Float -> Gloss.Picture
ballPic (V2 x y) =
  Gloss.translate x y $ circleFilled ballColor ballRadius

batPic :: Float -> Gloss.Picture
batPic x =
  Gloss.translate x batPositionY $ rectangleFilled batColor batWidth batHeight

brickPic :: Brick -> Gloss.Picture
brickPic (Brick (V2 x y) (Circle r)) =
  Gloss.translate x y $ circleFilled brickColor r
brickPic (Brick (V2 x y) (Rectangle w h)) =
  Gloss.translate x y $ rectangleFilled brickColor w h

lastCollPic ::
  Maybe (V2 Float, V2 Float, V2 Float, V2 Float) -> Gloss.Picture
lastCollPic = maybe Gloss.blank pics
  where
    pics (pos, before, normal, after) =
      let
        before' = (pos - 50 *^ normalize before, pos)
        normal' = (pos, pos + 50 *^ normal)
        after'  = (pos, pos + 50 *^ normalize after)
      in
        Gloss.pictures $ zipWith Gloss.color
          [Gloss.aquamarine, Gloss.chartreuse, Gloss.orange]
          (map (uncurry line) [before', normal', after'])
    line (V2 ux uy) (V2 vx vy) = Gloss.line [(ux, uy), (vx, vy)]

levelGeomPic :: LevelGeom -> Gloss.Picture
levelGeomPic (LevelGeom height offsetY rows) = heightPic <> rowsPic
  where
    heightPic = Gloss.color col
              . Gloss.translate 0 offsetY
              $ Gloss.rectangleWire (fromIntegral screenWidth) height
    rowsPic = Gloss.color col . Gloss.pictures $ map rowPic rows
    rowPic y = Gloss.line [(screenLeftBound, y), (screenRightBound, y)]
    col = Gloss.bright bgColor

circleFilled :: Gloss.Color -> Float -> Gloss.Picture
circleFilled color radius =
     Gloss.color color (Gloss.circleSolid radius)
  <> Gloss.color (Gloss.dark color) (Gloss.circle radius)

rectangleFilled :: Gloss.Color -> Float -> Float -> Gloss.Picture
rectangleFilled color width height =
     Gloss.color color (Gloss.rectangleSolid width height)
  <> Gloss.color (Gloss.dark color) (Gloss.rectangleWire width height)
