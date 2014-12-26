{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding (id, (.))
import Control.Wire
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Animate as Gloss
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


main :: IO ()
main = do
  refWire <- newIORef mainWire
  refTime <- newIORef 0

  let disp = Gloss.InWindow "breakout" (960, 540) (100, 100)
      bg   = Gloss.white
  Gloss.animateIO disp bg $ \currentTime -> do
    wire     <- readIORef refWire
    lastTime <- readIORef refTime

    let dTime           = currentTime - lastTime
        timed           = Timed dTime ()
        input           = Right ()
        (result, wire') = runIdentity $ stepWire wire timed input

    writeIORef refWire wire'
    writeIORef refTime currentTime
    return $ case result of
      Left  _   -> Gloss.blank
      Right pic -> pic
