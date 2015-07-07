{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Main (main) where

import Spanout.Common
import Spanout.Gameplay
import Spanout.Wire

import Control.Lens
import Control.Monad.Random
import Control.Monad.Reader

import qualified Data.Set as Set

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.ViewPort as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Linear

import qualified System.Exit as System (exitSuccess)



type MainWire = () ->> Gloss.Picture

data World = World
  { _worldWire    :: MainWire
  , _worldEnv     :: Env
  , _worldLastPic :: Gloss.Picture
  , _worldViewPort :: Gloss.ViewPort
  }

makeLenses ''World

main :: IO ()
main = Gloss.playIO disp Gloss.black fps world
    obtainPicture registerEvent performIteration
  where
    disp = Gloss.InWindow "spanout" winSize (0, 0)
    fps = 60
    winSize = (960, 540)
    world = World
      { _worldWire     = game
      , _worldEnv      = env
      , _worldLastPic  = Gloss.blank
      , _worldViewPort = viewPort winSize
      }
    env = Env
      { _envMouse = zero
      , _envKeys  = Set.empty
      }



registerEvent :: Gloss.Event -> World -> IO World
registerEvent (Gloss.EventResize wh) world =
  return $ set worldViewPort (viewPort wh) world
registerEvent (Gloss.EventMotion p) world =
  return $ set (worldEnv . envMouse) (V2 x y) world
  where
    (x, y) = Gloss.invertViewPort vp p
    vp = view worldViewPort world
registerEvent (Gloss.EventKey key Gloss.Down _ _) world =
  return $ over (worldEnv . envKeys) (Set.insert key) world
registerEvent (Gloss.EventKey key Gloss.Up _ _) world =
  return $ over (worldEnv . envKeys) (Set.delete key) world

performIteration :: Float -> World -> IO World
performIteration dTime world = do
  let
    timed = Timed dTime ()
    input = Right ()
    mb = stepWire (view worldWire world) timed input
  (epic, wire') <- evalRandIO . runReaderT mb $ view worldEnv world
  case epic of
    Right pic -> return $ set worldWire wire' . set worldLastPic pic $ world
    Left ()   -> System.exitSuccess

obtainPicture :: World -> IO Gloss.Picture
obtainPicture world = return $ Gloss.applyViewPortToPicture vp pic
  where
    vp = view worldViewPort world
    pic = view worldLastPic world

viewPort :: (Int, Int) -> Gloss.ViewPort
viewPort (w, h) = Gloss.viewPortInit { Gloss.viewPortScale = scale }
  where
    scale = min scaleX scaleY
    scaleX = fromIntegral w / 2 / screenBoundX
    scaleY = fromIntegral h / 2 / screenBoundY
