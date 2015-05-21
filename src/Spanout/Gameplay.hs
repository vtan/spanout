{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Gameplay
  ( Ball(..)
  , ballPos
  , ballVel
  , Brick(..)
  , GameState(..)
  , gsBall
  , gsBatX
  , gsBricks
  , gsLastCollision
  , countdownLogic
  , gameLogic
  , moveBat
  , exitOnEsc
  , ballAlive
  , collideBallEdge
  , collideBallBat
  , collideBallBrick
  , stateInit
  ) where

import Spanout.Common
import Spanout.Wire ((<+>))
import qualified Spanout.Wire as Wire

import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Lens
import Control.Monad
import Control.Monad.Random

import Data.Either (partitionEithers)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Linear



data Ball = Ball
  { _ballPos :: V2 Float
  , _ballVel :: V2 Float
  }
makeLenses ''Ball

data Brick
  = Circle (V2 Float) Float
  | Rectangle (V2 Float) Float Float

data GameState = GameState
  { _gsBall          :: Ball
  , _gsBatX          :: Float
  , _gsBricks        :: [Brick]
  , _gsLastCollision :: Maybe (V2 Float, V2 Float, V2 Float, V2 Float)
  }
makeLenses ''GameState

countdownLogic :: a ->> Either GameState (GameState, Float)
countdownLogic = proc _ -> do
  t <- Wire.time -< ()
  rec state <- moveBat <<< Wire.delayM stateInit -< state
  let remaining = countdownTime - t
  returnA -<
    if remaining <= 0
    then Left state
    else Right (state, remaining)

gameLogic :: GameState -> a ->> Maybe GameState
gameLogic initGs = proc _ -> do
  rec
    state' <- update <<< Wire.delay initGs -< state
    let state = fromJust state'
  returnA -< state'
  where
    update :: GameState ->> Maybe GameState
    update = over gsBall moveBall
         ^>> moveBat
           >>> collideBallBrick
           <+> collideBallEdge
           <+> collideBallBat
           <+> Wire.overI gsBall ballAlive
    moveBall (Ball pos vel) = Ball (pos + vel) vel

moveBat :: GameState ->> GameState
moveBat = proc gs -> do
  x <- Wire.constM . view $ envMouse . _x -< ()
  returnA -< set gsBatX x gs

exitOnEsc :: a ->> Maybe a
exitOnEsc = proc a -> do
  keys <- Wire.constM $ view envKeys -< ()
  returnA -<
    if Set.notMember esc keys
      then Just a
      else Nothing
  where
    esc = Gloss.SpecialKey Gloss.KeyEsc

ballAlive :: Ball ->> Maybe Ball
ballAlive = arr $ \ball ->
  if view (ballPos . _y) ball > screenLowerBound
  then Just ball
  else Nothing

collideBallEdge :: GameState ->> Maybe GameState
collideBallEdge = arr $ \gs -> do
  let
    ball = view gsBall gs
    pos = view ballPos ball
  normal <- ballEdgeNormal pos
  bouncedBall <- bounceBall ball normal
  let coll = (pos, view ballVel ball, normal, view ballVel bouncedBall)
  Just $
      set gsBall bouncedBall
    . set gsLastCollision (Just coll)
    $ gs

collideBallBat :: GameState ->> Maybe GameState
collideBallBat = arr $ \gs -> do
  let
    ball = view gsBall gs
    pos = view ballPos ball
  normal <- ballBatNormal (view gsBatX gs) pos
  bouncedBall <- bounceBall ball normal
  let coll = (pos, view ballVel ball, normal, view ballVel bouncedBall)
  Just $
      set gsBall bouncedBall
    . set gsLastCollision (Just coll)
    $ gs

collideBallBrick :: GameState ->> Maybe GameState
collideBallBrick = arr $ \gs -> do
  let
    ball = view gsBall gs
    check brick =
      case ballBrickNormal brick ball of
        Nothing     -> Left brick
        Just normal -> Right normal
    bricks' = map check $ view gsBricks gs
    (remBricks, collisionNormals) = partitionEithers bricks'
  guard . not . null $ collisionNormals
  let normal = normalize . sum $ collisionNormals
  bouncedBall <- bounceBall ball normal
  let coll = ( view ballPos ball
             , view ballVel ball
             , normal
             , view ballVel bouncedBall
             )
  Just $
      set gsBall bouncedBall
    . set gsBricks remBricks
    . set gsLastCollision (Just coll)
    $ gs

bounceBall :: Ball -> V2 Float -> Maybe Ball
bounceBall (Ball pos vel) normal
  | vel `dot` normal < 0 = Just $ Ball pos vel'
  | otherwise            = Nothing
  where
    vel' = reflect vel normal

ballEdgeNormal :: V2 Float -> Maybe (V2 Float)
ballEdgeNormal (V2 px py)
  | px <= screenLeftBound  = Just $  unit _x
  | px >= screenRightBound = Just $ -unit _x
  | py >= screenUpperBound = Just $ -unit _y
  | otherwise              = Nothing

ballBatNormal :: Float -> V2 Float -> Maybe (V2 Float)
ballBatNormal batX (V2 px py)
  | bxl && bxr && by = Just $ batNormal px batX
  | otherwise        = Nothing
  where
    bxl = px >= batX - batWidth / 2
    bxr = px <= batX + batWidth / 2
    by  = py <= batPositionY + batHeight / 2 + ballRadius

ballBrickNormal :: Brick -> Ball -> Maybe (V2 Float)
ballBrickNormal (Circle pos radius) ball
  | hit = Just . normalize $ view ballPos ball - pos
  | otherwise = Nothing
  where
    hit = distance (view ballPos ball) pos <= radius + ballRadius
ballBrickNormal (Rectangle pos@(V2 x y) width height) ball
  | tooFar = Nothing
  | hitX = Just $ signum (ballY - y) *^ unit _y
  | hitY = Just $ signum (ballX - x) *^ unit _x
  | hitCorner = Just . normalize $ signum <$> dist
  | otherwise = Nothing
  where
    dist = view ballPos ball - pos
    V2 distAbsX distAbsY = abs <$> dist
    V2 ballX ballY = view ballPos ball
    tooFar = distAbsX > width / 2 + ballRadius
          || distAbsY > height / 2 + ballRadius
    hitX = distAbsX <= width / 2
    hitY = distAbsY <= height / 2
    hitCorner = quadrance (V2 (distAbsX - width / 2) (distAbsY - height / 2))
             <= ballRadius ^ (2 :: Int)

batNormal :: Float -> Float -> V2 Float
batNormal x batX = perp . angle $ batSpread * relX
  where
    relX = (batX - x) / (batWidth / 2)

reflect :: Num a => V2 a -> V2 a -> V2 a
reflect vel normal = vel - (2 * vel `dot` normal) *^ normal

stateInit :: (MonadRandom m, Applicative m) => m GameState
stateInit = do
  shapeRatio <- getRandom
  bricks <- replicateM 20 $ do
    x <- getRandomR (screenLeftBound, screenRightBound)
    y <- getRandomR (screenLowerBound, screenUpperBound)
    let pos = V2 x y
    which <- getRandom
    if (which :: Double) < (shapeRatio :: Double)
    then Circle <$> pure pos <*> getRandomR (10, 50)
    else Rectangle <$> pure pos <*> getRandomR (50, 100) <*> getRandomR (10, 50)
  return GameState
    { _gsBall = Ball (V2 0 0) (V2 0 (-6))
    , _gsBatX = 0
    , _gsBricks = bricks
    , _gsLastCollision = Nothing
    }
