{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}

module Spanout.Gameplay (game) where

import Spanout.Common
import Spanout.Graphics
import Spanout.Level
import qualified Spanout.Wire as Wire

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad

import Data.Either (partitionEithers)
import Data.Maybe
import Data.Monoid

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Linear



game :: a ->> Gloss.Picture
game = Wire.bindW gsInit gameCountdown
  where
    gsInit = do
      bricks <- generateBricks
      return $ GameState
        { _gsBall = ballInit
        , _gsBatX = 0
        , _gsBricks = bricks
        }



gameCountdown :: GameState -> a ->> Gloss.Picture
gameCountdown gsInit = Wire.switch $ proc _ -> do
  batX <- Wire.constM . view $ envMouse . _x -< ()
  time <- Wire.time -< ()
  let
    gs = set gsBatX batX gsInit
    remainingTime = countdownTime - time
  returnA -< if
    | remainingTime > 0 -> Right $ gamePic gs <> countdownPic remainingTime
    | otherwise         -> Left  $ gameLevel gs

gameLevel :: GameState -> a ->> Gloss.Picture
gameLevel gsInit = Wire.switch $ proc _ -> do
  batX <- Wire.constM . view $ envMouse . _x -< ()
  rec
    ball' <- Wire.delay $ view gsBall gsInit -< ball
    bricks' <- Wire.delay $ view gsBricks gsInit -< bricks

    edgeNormal <- ballEdgeCollision -< ball'
    batNormal <- ballBatCollision -< (ball', batX)
    ballBrickColl <- ballBrickCollision -< (ball', bricks')
    let
      brickNormals = fst <$> ballBrickColl
      normal = mergeNormalEvents $
           maybeToList edgeNormal
        ++ maybeToList batNormal
        ++ fromMaybe [] brickNormals

    vel <- Wire.accumE reflect $ view (gsBall . ballVel) gsInit -< normal
    pos <- Wire.accum (\dt p v -> p + dt *^ v) $ view (gsBall . ballPos) gsInit
         -< vel
    let
      ball = Ball pos vel
      bricks = fromMaybe bricks' (snd <$> ballBrickColl)

  returnA -< if
    | null bricks ->
        Left $ game
    | view (ballPos . _y) ball <= -screenBoundY ->
        Left $ gameCountdown gsInit
    | otherwise ->
        Right . gamePic $ GameState
          { _gsBall = ball
          , _gsBatX = batX
          , _gsBricks = bricks
          }

mergeNormalEvents :: (Floating a, Epsilon a) => [V2 a] -> Maybe (V2 a)
mergeNormalEvents [] = Nothing
mergeNormalEvents normals = Just . normalize $ sum normals

ballEdgeCollision :: Ball ->> Maybe (V2 Float)
ballEdgeCollision = arr $ \(Ball pos vel) ->
  mfilter (oppositeDir vel) $ ballEdgeNormal pos

ballBatCollision :: (Ball, Float) ->> Maybe (V2 Float)
ballBatCollision = arr $ \(Ball pos vel, x) ->
  mfilter (oppositeDir vel) $ ballBatNormal x pos

ballBrickCollision :: (Ball, [Brick]) ->> Maybe ([V2 Float], [Brick])
ballBrickCollision = arr $ \(Ball pos vel, bricks) -> do
  let
    check brick =
      case ballBrickNormal brick pos of
        Just normal
          | oppositeDir vel normal -> Right normal
        _                          -> Left brick
    bricks' = map check bricks
    (remBricks, collisionNormals) = partitionEithers bricks'
  guard . not . null $ collisionNormals
  return (collisionNormals, remBricks)

ballEdgeNormal :: V2 Float -> Maybe (V2 Float)
ballEdgeNormal (V2 px py)
  | px <= -screenBoundX = Just $  unit _x
  | px >=  screenBoundX = Just $ -unit _x
  | py >=  screenBoundY = Just $ -unit _y
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
ballBrickNormal (Brick pos (Circle radius)) bpos
  | hit = Just . normalize $ bpos - pos
  | otherwise = Nothing
  where
    hit = distance bpos pos <= radius + ballRadius
ballBrickNormal (Brick pos@(V2 x y) (Rectangle width height)) bpos
  | tooFar = Nothing
  | hitX = Just $ signum (ballY - y) *^ unit _y
  | hitY = Just $ signum (ballX - x) *^ unit _x
  | otherwise = Nothing
  where
    dist = bpos - pos
    V2 distAbsX distAbsY = abs <$> dist
    V2 ballX ballY = bpos
    tooFar = distAbsX > width  / 2 + ballRadius
          || distAbsY > height / 2 + ballRadius
    hitX = distAbsX <= width  / 2 || (hitCorner && distAbsX > distAbsY)
    hitY = distAbsY <= height / 2 || (hitCorner && distAbsY > distAbsX)
    hitCorner = quadrance (V2 (distAbsX - width / 2) (distAbsY - height / 2))
             <= ballRadius ^ (2 :: Int)

batNormal :: Float -> Float -> V2 Float
batNormal x batX = perp . angle $ batSpread * relX
  where
    relX = (batX - x) / (batWidth / 2)

oppositeDir :: (Num a, Ord a) => V2 a -> V2 a -> Bool
oppositeDir vel normal = vel `dot` normal < 0

reflect :: Num a => V2 a -> V2 a -> V2 a
reflect vel normal = vel - (2 * vel `dot` normal) *^ normal
