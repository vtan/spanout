{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding (id, (.))
import Control.Wire
import Data.Maybe (fromMaybe)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Linear


type W a b = Wire (Timed Float ()) () Identity a b

type Ball = (V2 Float, V2 Float)

mainWire :: W Float Gloss.Picture
mainWire = proc mouseX -> do
  (V2 px py, V2 vx vy) <- ballWire -< ()
  let circ    = Gloss.circle ballRadius
      velLine = Gloss.line [(0, 0), (vx, vy)]
      ballPic = Gloss.translate px py . Gloss.pictures $ [circ, velLine]
      batPic  = Gloss.translate mouseX batPositionY
                $ Gloss.rectangleWire batWidth batHeight
  returnA -< Gloss.pictures [ballPic, batPic]

ballWire :: W () Ball
ballWire = proc () -> do
  rec
    (pos, vel) <- delay ballInit -< ball'
    let movedBall = (pos + vel, vel)
        ball'     = fromMaybe movedBall $ bounceBall movedBall
  returnA -< ball'
  where
    ballInit = (0, V2 ballInitialVelocityX ballInitialVelocityY)

bounceBall :: Ball -> Maybe Ball
bounceBall (pos, vel) = do
  normal <- ballBounceNormal pos
  vel' <- reflectIfNeeded vel normal
  return (pos, vel')

ballBounceNormal :: V2 Float -> Maybe (V2 Float)
ballBounceNormal (V2 px py)
  | px <= screenLeftBound  = Just $   unit _x
  | px >= screenRightBound = Just $ (-unit _x)
  | py <= screenLowerBound = Just $   unit _y
  | py >= screenUpperBound = Just $ (-unit _y)
  | otherwise              = Nothing

reflectIfNeeded :: V2 Float -> V2 Float -> Maybe (V2 Float)
reflectIfNeeded vel normal
  | dotprod < 0 = Just $ vel - (2 * dotprod) *^ normal
  | otherwise   = Nothing
  where
    dotprod = vel `dot` normal


type World = (W Float Gloss.Picture, Float, Gloss.Picture)

main :: IO ()
main = Gloss.play disp bg fps world obtainPicture registerEvent performIteration
  where
    disp  = Gloss.InWindow "breakout" (screenWidth, screenHeight) (100, 100)
    bg    = Gloss.white
    fps   = 60
    world = (mainWire, 0, Gloss.blank)

registerEvent :: Gloss.Event -> World -> World
registerEvent (Gloss.EventMotion (x, _y)) (wire, _x', pic) = (wire, x, pic)
registerEvent _event                      world            = world

performIteration :: Float -> World -> World
performIteration dTime (wire, mouseX, _lastPic) = (wire', mouseX, pic)
  where
    timed              = Timed dTime ()
    input              = Right mouseX
    (Right pic, wire') = runIdentity $ stepWire wire timed input

obtainPicture :: World -> Gloss.Picture
obtainPicture (_wire, _mouseX, pic) = pic


screenWidth :: Int
screenWidth = 960

screenHeight :: Int
screenHeight = 540

screenRightBound :: Float
screenRightBound = fromIntegral screenWidth / 2

screenLeftBound :: Float
screenLeftBound = (-screenRightBound)

screenUpperBound :: Float
screenUpperBound = fromIntegral screenHeight / 2

screenLowerBound :: Float
screenLowerBound = (-screenUpperBound)

ballRadius :: Float
ballRadius = 10

ballInitialVelocityX :: Float
ballInitialVelocityX = 3

ballInitialVelocityY :: Float
ballInitialVelocityY = 2

batWidth :: Float
batWidth = 160

batHeight :: Float
batHeight = 16

batPositionY :: Float
batPositionY = screenLowerBound + batHeight / 2
