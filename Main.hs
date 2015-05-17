{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Category
import Control.Lens
import Control.Monad (liftM)
import Control.Monad.Reader (Reader, runReader, ask)
import Control.Wire (Wire)
import qualified Control.Wire as Wire

import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, fromJust)

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss

import Linear



type a ->> b = Wire (Wire.Timed Float ()) () (Reader Float) a b

infixr 5 <+>
(<+>) :: (a ->> Maybe b) -> (a ->> Maybe b) -> (a ->> Maybe b)
(<+>) = liftA2 (<|>)

infix 4 <+|
(<+|) :: (a ->> Maybe b) -> (a ->> b) -> (a ->> b)
(<+|) = liftA2 $ flip fromMaybe

constM :: Monad m => m b -> Wire s e m a b
constM m = Wire.mkGen_ . const $ liftM Right m

overW :: Arrow p => Lens s s a a -> p a a -> p s s
overW l w = proc a -> do
  s <- w -< view l a
  returnA -< set l s a

overI :: Arrow p => Lens s s a a -> p a (Maybe a) -> p s (Maybe s)
overI l w = proc a -> do
  ms <- w -< view l a
  returnA -< fmap (\s -> set l s a) ms

infixr 9 .?
(.?) :: ArrowChoice p => p b c -> p a (Maybe b) -> p a (Maybe c)
p1 .? p2 = proc a -> do
  mb <- p2 -< a
  case mb of
    Just b  -> Just ^<< p1 -< b
    Nothing -> returnA -< Nothing

infixr 1 -->
(-->) :: (Monad m, Monoid s)
  => Wire s e m a (Maybe b) -> Wire s e m a b -> Wire s e m a b
w1 --> w2 = Wire.mkGen $ \s a -> do
  (Right mb, w1') <- Wire.stepWire w1 s (Right a)
  case mb of
    Nothing -> Wire.stepWire w2 s (Right a)
    Just b  -> return (Right b, w1' --> w2)

type Ball = (V2 Float, V2 Float)
type Brick = (V2 Float, Float)

data GameState = GameState
  { _gsBall :: Ball
  , _gsBricks :: [Brick]
  }
makeLenses ''GameState



mainWire :: a ->> Gloss.Picture
mainWire = (gameDisplay .? gameLogic) --> mainWire

gameDisplay :: GameState ->> Gloss.Picture
gameDisplay = proc gs -> do
  mouseX <- constM ask -< ()
  let
    V2 px py = view (gsBall . _1) gs
    ballPic = Gloss.translate px py $ Gloss.circle ballRadius
    batPic = Gloss.translate mouseX batPositionY
           $ Gloss.rectangleWire batWidth batHeight
    brickPics = [ Gloss.translate x y $ Gloss.circle r
                | (V2 x y, r) <- view gsBricks gs]
  returnA -< Gloss.pictures $ [ballPic, batPic] ++ brickPics

gameLogic :: a ->> Maybe GameState
gameLogic = proc _ -> do
  rec
    state' <- update . Wire.delay stateInit -< state
    let state = fromJust state'
  returnA -< state'
  where
    update = over  gsBall moveBall
         ^>>              collideBallBrick
         <+> overI gsBall collideBallEdge
         <+> overI gsBall collideBallBat
         <+> overI gsBall ballAlive
    moveBall (pos, vel) = (pos + vel, vel)

ballAlive :: Ball ->> Maybe Ball
ballAlive = arr $ \ball@(V2 _px py, _vel) ->
  if py > screenLowerBound
  then Just ball
  else Nothing

collideBallEdge :: Ball ->> Maybe Ball
collideBallEdge = arr $ \ball@(pos, _vel) ->
  bounceBall ball =<< ballEdgeNormal pos

collideBallBat :: Ball ->> Maybe Ball
collideBallBat = proc ball@(pos, _vel) -> do
  batX <- constM ask -< ()
  returnA -< bounceBall ball =<< ballBatNormal batX pos

collideBallBrick :: GameState ->> Maybe GameState
collideBallBrick = arr $ \gs -> do
  let
    check brick =
      case ballBrickNormal brick $ view (gsBall . _1) gs of
        Nothing     -> Left brick
        Just normal -> Right normal
    bricks' = map check $ view gsBricks gs
    (remBricks, collisionNormals) = partitionEithers bricks'
  case collisionNormals of
    [] -> Nothing
    _  ->
      let normal = normalize $ sum collisionNormals
      in  GameState <$> bounceBall (view gsBall gs) normal <*> Just remBricks



bounceBall :: Ball -> V2 Float -> Maybe Ball
bounceBall (pos, vel) normal
  | vel `dot` normal < 0 = Just (pos, vel')
  | otherwise            = Nothing
  where
    vel' = reflect vel normal

ballEdgeNormal :: V2 Float -> Maybe (V2 Float)
ballEdgeNormal (V2 px py)
  | px <= screenLeftBound  = Just $   unit _x
  | px >= screenRightBound = Just $ (-unit _x)
  | py >= screenUpperBound = Just $ (-unit _y)
  | otherwise              = Nothing

ballBatNormal :: Float -> V2 Float -> Maybe (V2 Float)
ballBatNormal batX (V2 px py)
  | bxl && bxr && by = Just $ batNormal px batX
  | otherwise        = Nothing
  where
    bxl = px >= batX - batWidth / 2
    bxr = px <= batX + batWidth / 2
    by  = py <= batPositionY + batHeight / 2 + ballRadius

ballBrickNormal :: Brick -> V2 Float -> Maybe (V2 Float)
ballBrickNormal (brickPos, brickRadius) pos
  | hit       = Just . normalize $ pos - brickPos
  | otherwise = Nothing
  where
    hit = distance brickPos pos <= brickRadius + ballRadius

batNormal :: Float -> Float -> V2 Float
batNormal x batX = perp . angle $ batSpread * relX
  where
    relX = (batX - x) / (batWidth / 2)

reflect :: Num a => V2 a -> V2 a -> V2 a
reflect vel normal = vel - (2 * vel `dot` normal) *^ normal



type World = (() ->> Gloss.Picture, Float, Gloss.Picture)

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

stateInit :: GameState
stateInit = GameState
  { _gsBall = (V2 0 0, V2 0 (-5))
  , _gsBricks =
    [ (V2  (-20) 100, 20)
    , (V2    20  100, 20)
    , (V2   250  150, 30)
    , (V2 (-250) 150, 30)
    ]
  }
