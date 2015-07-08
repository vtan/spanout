{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Gameplay (game) where

import Prelude hiding (id, (.))

import Spanout.Common
import Spanout.Graphics
import Spanout.Level
import qualified Spanout.Wire as Wire

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad

import Data.Either
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Linear



-- The reactive view of the game
game :: a ->> Gloss.Picture
game = Wire.bindW gsInit gameBegin
  where
    gsInit = do
      bricks <- generateBricks
      return GameState
        { _gsBall = ballInit
        , _gsBatX = 0
        , _gsBricks = bricks
        }



-- Displays the level and a countdown before the actual gameplay
gameBegin :: GameState -> a ->> Gloss.Picture
gameBegin gsInit = Wire.switch $ proc _ -> do
  batX <- view _x ^<< mousePos -< ()
  time <- Wire.time -< ()
  let
    gs = set gsBatX batX gsInit
    remainingTime = countdownTime - time
  returnA -< if
    | remainingTime > 0 -> Right $ gamePic gs <> countdownPic remainingTime
    | otherwise         -> Left  $ gameLevel gs

-- Gameplay from an initial game state
gameLevel :: GameState -> a ->> Gloss.Picture
gameLevel gsInit = Wire.switch $ proc _ -> do
  batX <- view _x ^<< mousePos -< ()
  rec
    -- Binding previous values
    ball' <- Wire.delay $ view gsBall gsInit -< ball
    bricks' <- Wire.delay $ view gsBricks gsInit -< bricks

    -- Current position
    pos <- Wire.accum (\dt p v -> p + dt *^ v) $ view (gsBall . ballPos) gsInit
        -< view ballVel ball'
    -- Collision and its normal
    let
      edgeNormal = ballEdgeNormal pos
      batNormal = ballBatNormal batX pos
      ballBrickColl = ballBrickCollision (ball' {_ballPos = pos}) bricks'
      brickNormals = fst <$> ballBrickColl
      normal = mfilter (faceAway $ view ballVel ball') . mergeNormalEvents $
           maybeToList edgeNormal
        ++ maybeToList batNormal
        ++ fromMaybe [] brickNormals
    -- Current velocity
    vel <- Wire.accumE reflect $ view (gsBall . ballVel) gsInit -< normal

    -- Binding current values
    let
      ball = Ball pos vel
      bricks = fromMaybe bricks' (snd <$> ballBrickColl)

  let gs = GameState {_gsBall = ball, _gsBatX = batX, _gsBricks = bricks}
  spacePressed <- keyPressed $ Gloss.SpecialKey Gloss.KeySpace -< ()
  returnA -< if
    | spacePressed ->
        Left game
    | null bricks ->
        Left $ levelEnd gs
    | view (ballPos . _y) ball <= -screenBoundY - ballRadius ->
        Left $ gameBegin gsInit
    | otherwise ->
        Right $ gamePic gs

-- Displays the final game state for some time after the end of the level
levelEnd :: GameState -> a ->> Gloss.Picture
levelEnd gs = Wire.switch $ Wire.forThen levelEndTime game . pure pic
  where
    pic = gamePic gs <> levelEndPic

-- The sum of zero or more normals
mergeNormalEvents :: (Floating a, Epsilon a) => [V2 a] -> Maybe (V2 a)
mergeNormalEvents [] = Nothing
mergeNormalEvents normals = Just . normalize $ sum normals

-- Collision between the ball and the screen edges
ballEdgeNormal :: V2 Float -> Maybe (V2 Float)
ballEdgeNormal (V2 px py)
  | px <= -screenBoundX + ballRadius = Just $  unit _x
  | px >=  screenBoundX - ballRadius = Just $ -unit _x
  | py >=  screenBoundY - ballRadius = Just $ -unit _y
  | otherwise                        = Nothing

-- Collision between the ball and the bat
ballBatNormal :: Float -> V2 Float -> Maybe (V2 Float)
ballBatNormal batX (V2 px py)
  | bxl && bxr && by = Just $ batNormalAt px batX
  | otherwise        = Nothing
  where
    bxl = px >= batX - batWidth / 2
    bxr = px <= batX + batWidth / 2
    by  = py <= batPositionY + batHeight / 2 + ballRadius

-- Collision between the ball and the bricks.
-- Calculates the resulting normals and the remaining bricks.
ballBrickCollision :: Ball -> [Brick] -> Maybe ([V2 Float], [Brick])
ballBrickCollision ball bricks =
  case collisionNormals of
    [] -> Nothing
    _  -> Just (collisionNormals, remBricks)
  where
    check brick =
      case ballBrickNormal brick ball of
        Just normal -> Right normal
        _           -> Left brick
    (remBricks, collisionNormals) = partitionEithers . map check $ bricks

-- Collision between the ball and a brick
ballBrickNormal :: Brick -> Ball -> Maybe (V2 Float)
ballBrickNormal (Brick pos (Circle radius)) (Ball bpos _)
  | hit = Just . normalize $ bpos - pos
  | otherwise = Nothing
  where
    hit = distance bpos pos <= radius + ballRadius
ballBrickNormal (Brick pos@(V2 x y) (Rectangle width height)) (Ball bpos bvel)
  | tooFar = Nothing
  | hitX = Just normalX
  | hitY = Just normalY
  | hitCorner = listToMaybe . filter (faceAway bvel) $ [normalX, normalY]
  | otherwise = Nothing
  where
    dist = bpos - pos
    V2 distAbsX distAbsY = abs <$> dist
    V2 ballX ballY = bpos
    tooFar = distAbsX > width  / 2 + ballRadius
          || distAbsY > height / 2 + ballRadius
    hitX = distAbsX <= width / 2
    hitY = distAbsY <= height / 2
    hitCorner = quadrance (V2 (distAbsX - width / 2) (distAbsY - height / 2))
             <= ballRadius ^ (2 :: Int)
    normalX = signum (ballY - y) *^ unit _y
    normalY = signum (ballX - x) *^ unit _x

-- The normal at a point of the bat
batNormalAt :: Float -> Float -> V2 Float
batNormalAt x batX = perp . angle $ batSpread * relX
  where
    relX = (batX - x) / (batWidth / 2)

-- Checks if two vectors face away from each other
faceAway :: (Num a, Ord a) => V2 a -> V2 a -> Bool
faceAway u v = u `dot` v < 0

-- The reflection of a vector based on a normal
reflect :: Num a => V2 a -> V2 a -> V2 a
reflect v normal = v - (2 * v `dot` normal) *^ normal

-- The reactive position of the mouse
mousePos :: a ->> V2 Float
mousePos = Wire.constM $ view envMouse

-- The reactive state of a keyboard button
keyPressed :: Gloss.Key -> a ->> Bool
keyPressed key = Wire.constM $ views envKeys (Set.member key)
