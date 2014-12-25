{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding (id, (.))
import Control.Wire
import Data.IORef
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Animate as Gloss


type W a b = Wire (Timed Float ()) () Identity a b

mainWire :: W () Gloss.Picture
mainWire = proc () -> do
  currentTime <- time -< ()
  let pic = Gloss.text . show $ currentTime
  returnA -< pic
--mainWire = time >>^ text . show

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
