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
type Brick = (V2 Float, Float)
type State = (Ball, Maybe Brick)

mainWire :: W () Gloss.Picture
mainWire = proc () -> do
  ((V2 px py, V2 vx vy), maybeBrick) <- gameWire -< ()
  mouseX <- Wire.mkGen_ (const (Right <$> ask)) -< ()
  let circ    = Gloss.circle ballRadius
      velLine = Gloss.line [(0, 0), (vx, vy)]
      ballPic = Gloss.translate px py . Gloss.pictures $ [circ, velLine]
      batPic  = Gloss.translate mouseX batPositionY
              $ Gloss.rectangleWire batWidth batHeight
      brickPic =
        case maybeBrick of
          Just (V2 bx by, r) -> Gloss.translate bx by $ Gloss.circle r
          Nothing            -> Gloss.blank
  returnA -< Gloss.pictures [ballPic, batPic, brickPic]

gameWire :: W () State
gameWire = proc () -> do
  rec
    state' <- Wire.delay stateInit -< state
    state <- updateGame -< state'
  returnA -< state

updateGame :: W State State
updateGame =
  moveBall
  ^>> collideBallBrick
  <|> first collideBallEdge
  <|> first collideBallBat
  <|> first (Wire.when ballAlive)
  <|> pure stateInit
  where
    moveBall ((pos, vel), maybeBrick) = ((pos + vel, vel), maybeBrick)
    ballAlive (V2 _px py, _vel) = py > screenLowerBound

collideBallEdge :: W Ball Ball
collideBallEdge = Wire.mkPure_ $ \ball@(pos, _vel) ->
  ballEdgeNormal pos >>= bounceBall ball

collideBallBat :: W Ball Ball
collideBallBat = Wire.mkGen_ $ \ball@(pos, _vel) -> do
  batX <- ask
  return $ ballBatNormal batX pos >>= bounceBall ball

collideBallBrick :: W State State
collideBallBrick = Wire.mkPure_ $ \(ball@(pos, _vel), maybeBrick) ->
  case maybeBrick of
    (Just brick) -> do
      ball' <- ballBrickNormal brick pos >>= bounceBall ball
      return (ball', Nothing)
    Nothing -> Left ()

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
  | bxl && bxr && by = Right $ batNormal px batX
  | otherwise        = Left ()
  where
    bxl = px >= batX - batWidth / 2
    bxr = px <= batX + batWidth / 2
    by  = py <= batPositionY + batHeight / 2 + ballRadius

ballBrickNormal :: Brick -> V2 Float -> Either () (V2 Float)
ballBrickNormal (brickPos, brickRadius) pos
  | hit       = Right . normalize $ pos - brickPos
  | otherwise = Left ()
  where
    hit = distance brickPos pos <= brickRadius + ballRadius

batNormal :: Float -> Float -> V2 Float
batNormal x batX = perp . angle $ batSpread * relX
  where
    relX = (batX - x) / (batWidth / 2)

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

batWidth :: Float
batWidth = 160

batHeight :: Float
batHeight = 16

batPositionY :: Float
batPositionY = screenLowerBound + batHeight / 2

batSpread :: Float
batSpread = pi / 6

stateInit :: State
stateInit = ((0, V2 0 (-5)), Just (V2 100 100, 20))
