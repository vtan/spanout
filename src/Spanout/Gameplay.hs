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
import qualified Data.Set as Set

import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Linear



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

gameLevel :: GameState -> a ->> Gloss.Picture
gameLevel gsInit = Wire.switch $ proc _ -> do
  batX <- view _x ^<< mousePos -< ()
  rec
    ball' <- Wire.delay $ view gsBall gsInit -< ball
    bricks' <- Wire.delay $ view gsBricks gsInit -< bricks

    let
      edgeNormal = ballEdgeNormal $ view ballPos ball'
      batNormal = ballBatNormal batX $ view ballPos ball'
      ballBrickColl = ballBrickCollision ball' bricks'
      brickNormals = fst <$> ballBrickColl
      normal = mfilter (oppositeDir $ view ballVel ball') . mergeNormalEvents $
           maybeToList edgeNormal
        ++ maybeToList batNormal
        ++ fromMaybe [] brickNormals

    vel <- Wire.accumE reflect $ view (gsBall . ballVel) gsInit -< normal
    pos <- Wire.accum (\dt p v -> p + dt *^ v) $ view (gsBall . ballPos) gsInit
         -< vel
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
    | view (ballPos . _y) ball <= -screenBoundY ->
        Left $ gameBegin gsInit
    | otherwise ->
        Right $ gamePic gs

levelEnd :: GameState -> a ->> Gloss.Picture
levelEnd gs = Wire.switch $ Wire.forThen levelEndTime game <<< pure pic
  where
    pic = gamePic gs <> levelEndPic

mergeNormalEvents :: (Floating a, Epsilon a) => [V2 a] -> Maybe (V2 a)
mergeNormalEvents [] = Nothing
mergeNormalEvents normals = Just . normalize $ sum normals

ballEdgeNormal :: V2 Float -> Maybe (V2 Float)
ballEdgeNormal (V2 px py)
  | px <= -screenBoundX = Just $  unit _x
  | px >=  screenBoundX = Just $ -unit _x
  | py >=  screenBoundY = Just $ -unit _y
  | otherwise              = Nothing

ballBatNormal :: Float -> V2 Float -> Maybe (V2 Float)
ballBatNormal batX (V2 px py)
  | bxl && bxr && by = Just $ batNormalAt px batX
  | otherwise        = Nothing
  where
    bxl = px >= batX - batWidth / 2
    bxr = px <= batX + batWidth / 2
    by  = py <= batPositionY + batHeight / 2 + ballRadius

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

ballBrickNormal :: Brick -> Ball -> Maybe (V2 Float)
ballBrickNormal (Brick pos (Circle radius)) (Ball bpos _)
  | hit = Just . normalize $ bpos - pos
  | otherwise = Nothing
  where
    hit = distance bpos pos <= radius + ballRadius
ballBrickNormal (Brick pos@(V2 x y) (Rectangle width height)) (Ball bpos bvel)
  | tooFar = Nothing
  | hitX = Just $ signum (ballY - y) *^ unit _y
  | hitY = Just $ signum (ballX - x) *^ unit _x
  | otherwise = Nothing
  where
    dist = bpos - pos
    V2 distAbsX distAbsY = abs <$> dist
    V2 velAbsX velAbsY = abs <$> bvel
    V2 ballX ballY = bpos
    tooFar = distAbsX > width  / 2 + ballRadius
          || distAbsY > height / 2 + ballRadius
    hitX = distAbsX <= width  / 2 || (hitCorner && velAbsX <= velAbsY)
    hitY = distAbsY <= height / 2 || (hitCorner && velAbsY <= velAbsX)
    hitCorner = quadrance (V2 (distAbsX - width / 2) (distAbsY - height / 2))
             <= ballRadius ^ (2 :: Int)

batNormalAt :: Float -> Float -> V2 Float
batNormalAt x batX = perp . angle $ batSpread * relX
  where
    relX = (batX - x) / (batWidth / 2)

oppositeDir :: (Num a, Ord a) => V2 a -> V2 a -> Bool
oppositeDir vel normal = vel `dot` normal < 0

reflect :: Num a => V2 a -> V2 a -> V2 a
reflect vel normal = vel - (2 * vel `dot` normal) *^ normal

mousePos :: a ->> V2 Float
mousePos = Wire.constM $ view envMouse

keyPressed :: Gloss.Key -> a ->> Bool
keyPressed key = Wire.constM $ views envKeys (Set.member key)
