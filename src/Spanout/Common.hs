{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Common
  ( Ball(..)
  , ballPos
  , ballVel
  , BrickGeom(..)
  , Brick(..)
  , brPos
  , brGeom
  , GameState(..)
  , gsBall
  , gsBatX
  , gsBricks
  , gsLastCollision
  , gsBrickRows

  , Env(..)
  , envMouse
  , envKeys
  , M
  , type (->>)

  , screenWidth
  , screenHeight
  , screenRightBound
  , screenLeftBound
  , screenUpperBound
  , screenLowerBound
  , ballRadius
  , batWidth
  , batHeight
  , batPositionY
  , batSpread
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



data Ball = Ball
  { _ballPos :: V2 Float
  , _ballVel :: V2 Float
  }
makeLenses ''Ball

data BrickGeom
  = Circle Float
  | Rectangle Float Float

data Brick = Brick
  { _brPos :: V2 Float
  , _brGeom :: BrickGeom
  }
makeLenses ''Brick

data GameState = GameState
  { _gsBall          :: Ball
  , _gsBatX          :: Float
  , _gsBricks        :: [Brick]
  , _gsLastCollision :: Maybe (V2 Float, V2 Float, V2 Float, V2 Float)
  , _gsBrickRows     :: [Float]
  }
makeLenses ''GameState

data Env = Env
  { _envMouse :: V2 Float
  , _envKeys  :: Set Gloss.Key
  }
makeLenses ''Env

type M = ReaderT Env (Rand StdGen)

type a ->> b = Wire (Wire.Timed Float ()) () M a b


screenWidth :: Int
screenWidth = 960

screenHeight :: Int
screenHeight = 540

screenRightBound :: Float
screenRightBound = fromIntegral screenWidth / 2

screenLeftBound :: Float
screenLeftBound = -screenRightBound

screenUpperBound :: Float
screenUpperBound = fromIntegral screenHeight / 2

screenLowerBound :: Float
screenLowerBound = -screenUpperBound

ballRadius :: Float
ballRadius = 10

batWidth :: Float
batWidth = 160

batHeight :: Float
batHeight = 16

batPositionY :: Float
batPositionY = screenLowerBound + batHeight / 2

batSpread :: Float
batSpread = pi / 12

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
