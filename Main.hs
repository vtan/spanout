{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding (id, (.))
import Control.Wire
import Data.IORef
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Animate as Gloss


type W a b = Wire (Timed Float ()) () Identity a b

type Ball = (Float, Float)

mainWire :: W () Gloss.Picture
mainWire = proc () -> do
  (pos, _vel) <- ballWire -< ()
  returnA -< Gloss.translate pos 0 $ Gloss.circle 10

ballWire :: W () Ball
ballWire = proc () -> do
  rec
    ball' <- delay ballInit -< ball
    ball <- bounceBall <<< moveBall -< ball'
  returnA -< ball
  where
    ballInit = (0, 1)

moveBall :: W Ball Ball
moveBall = arr $ \(pos, vel) -> (pos + vel, vel)

bounceBall :: W Ball Ball
bounceBall = arr $ \(pos, vel) ->
  if pos <= -480 && vel < 0 || pos >= 480 && vel > 0
  then (pos, -vel)
  else (pos, vel)

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
