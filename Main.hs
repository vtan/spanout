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

mainWire :: W () Gloss.Picture
mainWire = proc () -> do
  (V2 px py, V2 vx vy) <- ballWire -< ()
  let circ    = Gloss.circle 10
      velLine = Gloss.line [(0, 0), (vx, vy)]
  returnA -< Gloss.translate px py . Gloss.pictures $ [circ, velLine]

ballWire :: W () Ball
ballWire = proc () -> do
  rec
    (pos, vel) <- delay ballInit -< ball'
    let movedBall = (pos + vel, vel)
        ball'     = fromMaybe movedBall $ bounceBall movedBall
  returnA -< ball'
  where
    ballInit = (0, V2 3 2)

bounceBall :: Ball -> Maybe Ball
bounceBall (pos, vel) = do
  normal <- ballBounceNormal pos
  vel' <- reflectIfNeeded vel normal
  return (pos, vel')

ballBounceNormal :: V2 Float -> Maybe (V2 Float)
ballBounceNormal (V2 px py)
  | px <= -480 = Just $ V2   1   0
  | px >=  480 = Just $ V2 (-1)  0
  | py <= -270 = Just $ V2   0   1
  | py >=  270 = Just $ V2   0 (-1)
  | otherwise  = Nothing

reflectIfNeeded :: V2 Float -> V2 Float -> Maybe (V2 Float)
reflectIfNeeded vel normal
  | dotprod < 0 = Just $ vel - (2 * dotprod) *^ normal
  | otherwise   = Nothing
  where
    dotprod = vel `dot` normal


type World = (W () Gloss.Picture, Gloss.Picture)

main :: IO ()
main = Gloss.play disp bg fps world obtainPicture registerEvent performIteration
  where
    disp  = Gloss.InWindow "breakout" (960, 540) (100, 100)
    bg    = Gloss.white
    fps   = 60
    world = (mainWire, Gloss.blank)

registerEvent :: Gloss.Event -> World -> World
registerEvent _event world = world

performIteration :: Float -> World -> World
performIteration dTime (wire, _lastPic) = (wire', pic)
  where
    timed              = Timed dTime ()
    input              = Right ()
    (Right pic, wire') = runIdentity $ stepWire wire timed input

obtainPicture :: World -> Gloss.Picture
obtainPicture (_wire, pic) = pic
