{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Graphics
  ( gameDisplay
  , countdownDisplay
  ) where

import Spanout.Common
import Spanout.Gameplay
import qualified Spanout.Wire as Wire

import Control.Arrow
import Control.Lens

import Data.Monoid ((<>))

import qualified Graphics.Gloss as Gloss

import Linear



gameDisplay :: GameState ->> Gloss.Picture
gameDisplay = proc gs -> do
  mouseX <- Wire.constM . view $ envMouse . _x -< ()
  let
    V2 px py = view (gsBall . ballPos) gs
    ballPic = Gloss.translate px py $ circleFilled ballColor ballRadius
    batPic = Gloss.translate mouseX batPositionY
           $ rectangleFilled batColor batWidth batHeight
    brickPics = map brickPic $ view gsBricks gs
    lastCollPic = displayLastColl $ view gsLastCollision gs
  returnA -< Gloss.pictures $ brickPics ++ [ballPic, batPic, lastCollPic]

brickPic :: Brick -> Gloss.Picture
brickPic (Circle (V2 x y) r) =
  Gloss.translate x y $ circleFilled brickColor r
brickPic (Rectangle (V2 x y) w h) =
  Gloss.translate x y $ rectangleFilled brickColor w h


countdownDisplay :: (GameState, Float) ->> Gloss.Picture
countdownDisplay = proc (gs, remaining) -> do
  pic <- gameDisplay -< gs
  let text = show (ceiling remaining :: Int)
  returnA -< pic <> (Gloss.color Gloss.chartreuse . Gloss.text $ text)


circleFilled :: Gloss.Color -> Float -> Gloss.Picture
circleFilled color radius =
     Gloss.color color (Gloss.circleSolid radius)
  <> Gloss.color (Gloss.dark color) (Gloss.circle radius)

rectangleFilled :: Gloss.Color -> Float -> Float -> Gloss.Picture
rectangleFilled color width height =
     Gloss.color color (Gloss.rectangleSolid width height)
  <> Gloss.color (Gloss.dark color) (Gloss.rectangleWire width height)

displayLastColl ::
  Maybe (V2 Float, V2 Float, V2 Float, V2 Float) -> Gloss.Picture
displayLastColl = maybe Gloss.blank pics
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
