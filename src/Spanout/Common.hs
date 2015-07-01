{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Common
  ( M
  , type (->>)

  , GameState(..)
  , gsBall
  , gsBatX
  , gsBricks
  , Ball(..)
  , ballPos
  , ballVel
  , Brick(..)
  , brPos
  , brGeom
  , BrickGeom(..)

  , Env(..)
  , envMouse
  , envKeys

  , screenWidth
  , screenHeight
  , screenBoundX
  , screenBoundY
  , ballRadius
  , ballInit
  , batWidth
  , batHeight
  , batPositionY
  , batSpread
  , brickWidth
  , brickHeight
  , countdownTime
  , levelEndTime
  , bgColor
  , ballColor
  , batColor
  , brickColor
  , textScale
  , textColor
  ) where

import Spanout.Wire

import Control.Lens
import Control.Monad.Random
import Control.Monad.Reader

import Data.Set (Set)

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Linear



type M = ReaderT Env (Rand StdGen)

type a ->> b = Wire (Timed Float ()) () M a b

data GameState = GameState
  { _gsBall          :: Ball
  , _gsBatX          :: Float
  , _gsBricks        :: [Brick]
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

data Env = Env
  { _envMouse :: V2 Float
  , _envKeys  :: Set Gloss.Key
  }

makeLenses ''GameState
makeLenses ''Ball
makeLenses ''Brick
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

ballInit :: Ball
ballInit = Ball
  { _ballPos = V2 0 (-screenBoundY + 4 * batHeight)
  , _ballVel = V2 0 (-1.7)
  }

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

levelEndTime :: Float
levelEndTime = 1.5

bgColor :: Gloss.Color
bgColor = Gloss.makeColor8 0xf5 0xe9 0xc2 0xff

ballColor :: Gloss.Color
ballColor = Gloss.makeColor8 0x67 0x4c 0xb7 0xff

batColor :: Gloss.Color
batColor = Gloss.makeColor8 0x9d 0x90 0x64 0xff

brickColor :: Gloss.Color
brickColor = Gloss.makeColor8 0x63 0xb8 0xc3 0xff

textScale :: Float
textScale = 0.004

textColor :: Gloss.Color
textColor = Gloss.greyN 0.3
