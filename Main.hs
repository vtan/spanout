{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding (id, (.))
import Control.Wire
import qualified Data.Bifunctor as Bifunctor
import Data.Maybe (isJust, fromJust)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Linear


type W a b = Wire (Timed Float ()) () Identity a b

type Ball = (V2 Float, V2 Float)

mainWire :: W Float Gloss.Picture
mainWire = proc mouseX -> do
  (V2 px py, V2 vx vy) <- ballWire -< mouseX
  let circ    = Gloss.circle ballRadius
      velLine = Gloss.line [(0, 0), (vx, vy)]
      ballPic = Gloss.translate px py . Gloss.pictures $ [circ, velLine]
      batPic  = Gloss.translate mouseX batPositionY
              $ Gloss.rectangleWire batWidth batHeight
  returnA -< Gloss.pictures [ballPic, batPic]

ballWire :: W Float Ball
ballWire = proc batX -> do
  rec
    ball' <- delay ballInit -<  ball
    ball  <- updateBall     -< (ball', batX)
  returnA -< ball

updateBall :: W (Ball, Float) Ball
updateBall =
  Bifunctor.first moveBall
  ^>> (fst ^>> arr bounceBall >>> when isJust >>^ fromJust)
  <|> (arr (uncurry collideBallBat) >>> when isJust >>^ fromJust)
  <|> (fst ^>> when ballAlive)
  <|> (fst ^>> arr (const ballInit))
  where
    moveBall (pos, vel) = (pos + vel, vel)
    ballAlive (V2 _px py, _vel) = py > screenLowerBound

bounceBall :: Ball -> Maybe Ball
bounceBall (pos, vel) = do
  normal <- ballBounceNormal pos
  vel' <- reflectIfNeeded vel normal
  return (pos, vel')

collideBallBat :: Ball -> Float -> Maybe Ball
collideBallBat (pos, vel) batX = do
  normal <- ballBatCollisionNormal batX pos
  vel' <- reflectIfNeeded vel normal
  return (pos, vel')

ballBounceNormal :: V2 Float -> Maybe (V2 Float)
ballBounceNormal (V2 px py)
  | px <= screenLeftBound  = Just $   unit _x
  | px >= screenRightBound = Just $ (-unit _x)
  | py >= screenUpperBound = Just $ (-unit _y)
  | otherwise              = Nothing

ballBatCollisionNormal :: Float -> V2 Float -> Maybe (V2 Float)
ballBatCollisionNormal batX (V2 px py)
  | bxl && bxr && by = Just $ unit _y
  | otherwise        = Nothing
  where
    bxl = px >= batX - batWidth / 2
    bxr = px <= batX + batWidth / 2
    by  = py <= batPositionY + batHeight / 2 + ballRadius

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

ballInit :: Ball
ballInit = (0, V2 3 2)

batWidth :: Float
batWidth = 160

batHeight :: Float
batHeight = 16

batPositionY :: Float
batPositionY = screenLowerBound + batHeight / 2
