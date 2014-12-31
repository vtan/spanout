{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Reader (Reader, runReader, ask)
import qualified Control.Wire as Wire

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss

import Linear


type W a b = Wire.Wire (Wire.Timed Float ()) () (Reader Float) a b

type Ball = (V2 Float, V2 Float)

mainWire :: W () Gloss.Picture
mainWire = proc () -> do
  (V2 px py, V2 vx vy) <- ballWire -< ()
  mouseX <- Wire.mkGen_ (const (Right <$> ask)) -< ()
  let circ    = Gloss.circle ballRadius
      velLine = Gloss.line [(0, 0), (vx, vy)]
      ballPic = Gloss.translate px py . Gloss.pictures $ [circ, velLine]
      batPic  = Gloss.translate mouseX batPositionY
              $ Gloss.rectangleWire batWidth batHeight
  returnA -< Gloss.pictures [ballPic, batPic]

ballWire :: W () Ball
ballWire = proc () -> do
  rec
    ball' <- Wire.delay ballInit -< ball
    ball <- updateBall -< ball'
  returnA -< ball

updateBall :: W Ball Ball
updateBall =
  moveBall
  ^>> ballEdgeCollisionWire
  <|> ballBatCollisionWire
  <|> Wire.when ballAlive
  <|> arr (const ballInit)
  where
    moveBall (pos, vel) = (pos + vel, vel)
    ballAlive (V2 _px py, _vel) = py > screenLowerBound

ballEdgeCollisionWire :: W Ball Ball
ballEdgeCollisionWire = Wire.mkPure_ $ \ball@(pos, _vel) ->
  ballEdgeNormal pos >>= bounceBall ball

ballBatCollisionWire :: W Ball Ball
ballBatCollisionWire = Wire.mkGen_ $ \ball@(pos, _vel) -> do
  batX <- ask
  return $ ballBatNormal batX pos >>= bounceBall ball

bounceBall :: Ball -> V2 Float -> Either () Ball
bounceBall (pos, vel) normal
  | vel `dot` normal < 0 = Right (pos, vel')
  | otherwise            = Left ()
  where
    vel' = reflect vel normal

ballEdgeNormal :: V2 Float -> Either () (V2 Float)
ballEdgeNormal (V2 px py)
  | px <= screenLeftBound  = Right $   unit _x
  | px >= screenRightBound = Right $ (-unit _x)
  | py >= screenUpperBound = Right $ (-unit _y)
  | otherwise              = Left ()

ballBatNormal :: Float -> V2 Float -> Either () (V2 Float)
ballBatNormal batX (V2 px py)
  | bxl && bxr && by = Right $ unit _y
  | otherwise        = Left ()
  where
    bxl = px >= batX - batWidth / 2
    bxr = px <= batX + batWidth / 2
    by  = py <= batPositionY + batHeight / 2 + ballRadius

reflect :: Num a => V2 a -> V2 a -> V2 a
reflect vel normal = vel - (2 * vel `dot` normal) *^ normal


type World = (W () Gloss.Picture, Float, Gloss.Picture)

main :: IO ()
main = Gloss.play disp bg fps world obtainPicture registerEvent performIteration
  where
    disp  = Gloss.InWindow "breakout" (screenWidth, screenHeight) (100, 100)
    bg    = Gloss.white
    fps   = 60
    world = (mainWire, 0, Gloss.blank)

registerEvent :: Gloss.Event -> World -> World
registerEvent (Gloss.EventMotion (x, _y)) (wire, _x', pic) = (wire, x, pic)
registerEvent _event                      world            = world

performIteration :: Float -> World -> World
performIteration dTime (wire, mouseX, _lastPic) = (wire', mouseX, pic)
  where
    timed              = Wire.Timed dTime ()
    input              = Right ()
    (Right pic, wire') = runReader (Wire.stepWire wire timed input) mouseX

obtainPicture :: World -> Gloss.Picture
obtainPicture (_wire, _mouseX, pic) = pic


screenWidth :: Int
screenWidth = 960

screenHeight :: Int
screenHeight = 540

screenRightBound :: Float
screenRightBound = fromIntegral screenWidth / 2

screenLeftBound :: Float
screenLeftBound = (-screenRightBound)

screenUpperBound :: Float
screenUpperBound = fromIntegral screenHeight / 2

screenLowerBound :: Float
screenLowerBound = (-screenUpperBound)

ballRadius :: Float
ballRadius = 10

ballInit :: Ball
ballInit = (0, V2 3 2)

batWidth :: Float
batWidth = 160

batHeight :: Float
batHeight = 16

batPositionY :: Float
batPositionY = screenLowerBound + batHeight / 2
