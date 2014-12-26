{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding (id, (.))
import Control.Wire
import Data.IORef
import Data.Maybe (fromJust, isJust)
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
    ball' <- delay ballInit -< ball
    ball <- moveAndMaybeBounce -< ball'
  returnA -< ball
  where
    ballInit = (0, V2 3 2)
    moveAndMaybeBounce = (bounceBall <<< moveBall) <|> moveBall

moveBall :: W Ball Ball
moveBall = arr $ \(pos, vel) -> (pos + vel, vel)

bounceBall :: W Ball Ball
bounceBall = proc ball@(pos, vel) -> do
  normal <- arrJust bounceNormal -< ball
  reflectedVel <- arrJust $ uncurry reflectIfNeeded -< (vel, normal)
  returnA -< (pos, reflectedVel)

bounceNormal :: Ball -> Maybe (V2 Float)
bounceNormal (V2 px py, _vel)
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


arrJust :: (Monoid e, Monad m) => (a -> Maybe b) -> Wire s e m a b
arrJust f = arr fromJust <<< when isJust <<< arr f

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
