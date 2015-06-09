{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Main (main) where

import Spanout.Common
import Spanout.Gameplay
import Spanout.Graphics
import Spanout.Wire ((=>>>=), (=>>>))
import qualified Spanout.Wire as Wire

import Control.Lens
import Control.Monad.Random
import Control.Monad.Reader

import qualified Data.Set as Set

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Linear

import qualified System.Exit as System (exitSuccess)



type MainWire = () ->> Maybe Gloss.Picture

mainWire :: MainWire
mainWire = exitOnEsc =>>>= countdown'
  where
    countdown' :: a ->> Maybe Gloss.Picture
    countdown' = Wire.switch $ (countdownLogic =>>> countdownDisplay) `Wire.choose` game'
    game' :: GameState -> a ->> Maybe Gloss.Picture
    game' k = Wire.switch $ (gameLogic k =>>> gameDisplay) `Wire.followedBy` countdown'

data World = World
  { _worldWire    :: MainWire
  , _worldEnv     :: Env
  , _worldLastPic :: Gloss.Picture
  }

makeLenses ''World

main :: IO ()
main = Gloss.playIO disp bgColor fps world obtainPicture registerEvent performIteration
  where
    disp = Gloss.InWindow "breakout" (screenWidth, screenHeight) (100, 100)
    fps = 60
    world = World
      { _worldWire    = mainWire
      , _worldEnv     = env
      , _worldLastPic = Gloss.blank
      }
    env = Env
      { _envMouse = zero
      , _envKeys  = Set.empty
      }



registerEvent :: Gloss.Event -> World -> IO World
registerEvent (Gloss.EventMotion (x, y)) world =
  return $ set (worldEnv . envMouse) (V2 x y) world
registerEvent (Gloss.EventKey key Gloss.Down _ _) world =
  return $ over (worldEnv . envKeys) (Set.insert key) world
registerEvent (Gloss.EventKey key Gloss.Up _ _) world =
  return $ over (worldEnv . envKeys) (Set.delete key) world
registerEvent _event world =
  return world

performIteration :: Float -> World -> IO World
performIteration dTime world = do
  let
    timed = Wire.Timed dTime ()
    input = Right ()
    mb = Wire.stepWire (view worldWire world) timed input
  (Right mpic, wire') <- evalRandIO . runReaderT mb $ view worldEnv world
  case mpic of
    Just pic -> return $ set worldWire wire' . set worldLastPic pic $ world
    Nothing  -> System.exitSuccess

obtainPicture :: World -> IO Gloss.Picture
obtainPicture = return . view worldLastPic
