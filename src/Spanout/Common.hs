{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Common
  ( M
  , type (->>)

  , GameState(..)
  , gsBall
  , gsBatX
  , gsBricks
  , gsLastCollision
  , gsLevelGeom
  , Ball(..)
  , ballPos
  , ballVel
  , Brick(..)
  , brPos
  , brGeom
  , BrickGeom(..)
  , LevelGeom(..)
  , lvlgeomHeight
  , lvlgeomOffsetY
  , lvlgeomRowHeights

  , Env(..)
  , envMouse
  , envKeys

  , screenWidth
  , screenHeight
  , screenBoundX
  , screenBoundY
  , ballRadius
  , batWidth
  , batHeight
  , batPositionY
  , batSpread
  , brickWidth
  , brickHeight
  , countdownTime
  , bgColor
  , ballColor
  , batColor
  , brickColor
  ) where

import Control.Lens
import Control.Monad.Random
import Control.Monad.Reader
import Control.Wire (Wire)
import qualified Control.Wire as Wire

import Data.Set (Set)

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Linear



type M = ReaderT Env (Rand StdGen)

type a ->> b = Wire (Wire.Timed Float ()) () M a b

data GameState = GameState
  { _gsBall          :: Ball
  , _gsBatX          :: Float
  , _gsBricks        :: [Brick]
  , _gsLastCollision :: Maybe (V2 Float, V2 Float, V2 Float, V2 Float)
  , _gsLevelGeom     :: LevelGeom
  }

data Ball = Ball
  { _ballPos :: V2 Float
  , _ballVel :: V2 Float
  }

data Brick = Brick
  { _brPos  :: V2 Float
  , _brGeom :: BrickGeom
  }

data BrickGeom
  = Circle Float
  | Rectangle Float Float

data LevelGeom = LevelGeom
  { _lvlgeomHeight     :: Float
  , _lvlgeomOffsetY    :: Float
  , _lvlgeomRowHeights :: [Float]
  }

data Env = Env
  { _envMouse :: V2 Float
  , _envKeys  :: Set Gloss.Key
  }

makeLenses ''GameState
makeLenses ''Ball
makeLenses ''Brick
makeLenses ''LevelGeom
makeLenses ''Env

screenWidth :: Float
screenWidth = 2 * (16 / 9)

screenHeight :: Float
screenHeight = 2

screenBoundX :: Float
screenBoundX = screenWidth / 2

screenBoundY :: Float
screenBoundY = screenHeight / 2

ballRadius :: Float
ballRadius = 0.04

batWidth :: Float
batWidth = 0.5

batHeight :: Float
batHeight = 0.05

batPositionY :: Float
batPositionY = -screenBoundY + batHeight / 2

batSpread :: Float
batSpread = pi / 12

brickWidth :: Float
brickWidth = 0.3

brickHeight :: Float
brickHeight = 0.1


countdownTime :: Float
countdownTime = 3

bgColor :: Gloss.Color
bgColor = Gloss.greyN 0.15

ballColor :: Gloss.Color
ballColor = Gloss.makeColor8 0x69 0x9a 0x33 0xff

batColor :: Gloss.Color
batColor = Gloss.makeColor8 0x5e 0x85 0x9a 0xff

brickColor :: Gloss.Color
brickColor = Gloss.makeColor8 0xaa 0x52 0x39 0xff
