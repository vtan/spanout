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
import Control.Monad (guard, liftM, replicateM)
import Control.Monad.Random (MonadRandom, Rand, StdGen, evalRandIO, getRandomR)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Wire (Wire)
import qualified Control.Wire as Wire

import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.IO.Game as Gloss

import Linear

import qualified System.Exit as System (exitSuccess)



data Env = Env
  { _envMouse :: V2 Float
  , _envKeys  :: Set Gloss.Key
  }
makeLenses ''Env

type M = ReaderT Env (Rand StdGen)
type a ->> b = Wire (Wire.Timed Float ()) () M a b

infixr 5 <+>
(<+>) :: (a ->> Maybe b) -> (a ->> Maybe b) -> (a ->> Maybe b)
(<+>) = liftA2 (<|>)

infix 4 <+|
(<+|) :: (a ->> Maybe b) -> (a ->> b) -> (a ->> b)
(<+|) = liftA2 $ flip fromMaybe

constM :: Monad m => m b -> Wire s e m a b
constM m = Wire.mkGen_ . const $ liftM Right m

delayM :: Monad m => m a -> Wire s e m a a
delayM ma = Wire.mkGenN $ \a' -> do
  a <- ma
  return (Right a, Wire.delay a')

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

infixr 9 ?.?
(?.?) :: ArrowChoice p => p b (Maybe c) -> p a (Maybe b) -> p a (Maybe c)
p1 ?.? p2 = proc a -> do
  mb <- p2 -< a
  case mb of
    Just b  -> p1 -< b
    Nothing -> returnA -< Nothing

infixr 1 ?>>>
(?>>>) :: ArrowChoice p => p a (Maybe b) -> p b c -> p a (Maybe c)
(?>>>) = flip (.?)

infixr 1 ?>>>?
(?>>>?) :: ArrowChoice p => p a (Maybe b) -> p b (Maybe c) -> p a (Maybe c)
(?>>>?) = flip (?.?)

infixr 1 ^>>?
(^>>?) :: Arrow p => (a -> b) -> p b (Maybe c) -> p a (Maybe c)
f ^>>? p = arr f >>> p

infixr 1 -->
(-->) :: (Monad m, Monoid s)
  => Wire s e m a (Maybe b) -> Wire s e m a (Maybe b) -> Wire s e m a (Maybe b)
w1 --> w2 = Wire.mkGen $ \s a -> do
  (Right mb, w1') <- Wire.stepWire w1 s (Right a)
  case mb of
    Nothing -> Wire.stepWire w2 s (Right a)
    Just b  -> return (Right $ Just b, w1' --> w2)

data Ball = Ball
  { _ballPos :: V2 Float
  , _ballVel :: V2 Float
  }
makeLenses ''Ball

data Brick = Brick
  { _brickCenter :: V2 Float
  , _brickRadius :: Float
  }
makeLenses ''Brick

data GameState = GameState
  { _gsBall          :: Ball
  , _gsBatX          :: Float
  , _gsBricks        :: [Brick]
  , _gsLastCollision :: Maybe (V2 Float, V2 Float, V2 Float, V2 Float)
  }
makeLenses ''GameState



mainWire :: a ->> Maybe Gloss.Picture
mainWire = exitOnEsc ?>>>? (gameLogic ?>>> gameDisplay) --> mainWire

gameDisplay :: GameState ->> Gloss.Picture
gameDisplay = proc gs -> do
  mouseX <- constM . view $ envMouse . _x -< ()
  let
    V2 px py = view (gsBall . ballPos) gs
    ballPic = Gloss.translate px py $ circleFilled ballColor ballRadius
    batPic = Gloss.translate mouseX batPositionY
           $ rectangleFilled batColor batWidth batHeight
    brickPics = [ Gloss.translate x y $ circleFilled brickColor r
                | Brick (V2 x y) r <- view gsBricks gs]
    lastCollPic = displayLastColl $ view gsLastCollision gs
  returnA -< Gloss.pictures $ brickPics ++ [ballPic, batPic, lastCollPic]

gameLogic :: a ->> Maybe GameState
gameLogic = proc _ -> do
  rec
    state' <- update . delayM stateInit -< state
    let state = fromJust state'
  returnA -< state'
  where
    update :: GameState ->> Maybe GameState
    update = over gsBall moveBall
         ^>> moveBat
           >>> collideBallBrick
           <+> collideBallEdge
           <+> collideBallBat
           <+> overI gsBall ballAlive
    moveBall (Ball pos vel) = Ball (pos + vel) vel
    moveBat :: GameState ->> GameState
    moveBat = proc gs -> do
      x <- constM . view $ envMouse . _x -< ()
      returnA -< set gsBatX x gs

exitOnEsc :: a ->> Maybe a
exitOnEsc = proc a -> do
  keys <- constM $ view envKeys -< ()
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

ballBrickNormal :: Brick -> Ball -> Maybe (V2 Float)
ballBrickNormal (Brick pos radius) ball
  | hit       = Just . normalize $ (view ballPos ball) - pos
  | otherwise = Nothing
  where
    hit = distance (view ballPos ball) pos <= radius + ballRadius

batNormal :: Float -> Float -> V2 Float
batNormal x batX = perp . angle $ batSpread * relX
  where
    relX = (batX - x) / (batWidth / 2)

reflect :: Num a => V2 a -> V2 a -> V2 a
reflect vel normal = vel - (2 * vel `dot` normal) *^ normal

circleFilled :: Gloss.Color -> Float -> Gloss.Picture
circleFilled color radius =
     Gloss.color color (Gloss.circleSolid radius)
  <> Gloss.color (Gloss.dark color) (Gloss.circle radius)

rectangleFilled :: Gloss.Color -> Float -> Float -> Gloss.Picture
rectangleFilled color width height =
     Gloss.color color (Gloss.rectangleSolid width height)
  <> Gloss.color (Gloss.dark color) (Gloss.rectangleWire width height)

displayLastColl ::
  Maybe (V2 Float, V2 Float, V2 Float, V2 Float) -> Gloss.Picture
displayLastColl = fromMaybe Gloss.blank . fmap pics
  where
    pics (pos, before, normal, after) =
      let
        before' = (pos - 50 *^ normalize before, pos)
        normal' = (pos, pos + 50 *^ normal)
        after'  = (pos, pos + 50 *^ normalize after)
      in
        Gloss.pictures $ zipWith Gloss.color
          [Gloss.aquamarine, Gloss.chartreuse, Gloss.orange]
          (map (uncurry line) [before', normal', after'])
    line (V2 ux uy) (V2 vx vy) = Gloss.line [(ux, uy), (vx, vy)]



type World = (() ->> Maybe Gloss.Picture, Env, Gloss.Picture)

main :: IO ()
main = Gloss.playIO disp bgColor fps world obtainPicture registerEvent performIteration
  where
    disp = Gloss.InWindow "breakout" (screenWidth, screenHeight) (100, 100)
    fps = 60
    world = (mainWire, env, Gloss.blank)
    env = Env
      { _envMouse = zero
      , _envKeys  = Set.empty
      }


registerEvent :: Gloss.Event -> World -> IO World
registerEvent (Gloss.EventMotion (x, y)) world =
  return $ set (_2 . envMouse) (V2 x y) world
registerEvent (Gloss.EventKey key Gloss.Down _ _) world =
  return $ over (_2 . envKeys) (Set.insert key) world
registerEvent (Gloss.EventKey key Gloss.Up _ _) world =
  return $ over (_2 . envKeys) (Set.delete key) world
registerEvent _event world =
  return world

performIteration :: Float -> World -> IO World
performIteration dTime (wire, mouseX, _lastPic) = do
  let
    timed = Wire.Timed dTime ()
    input = Right ()
    mb = Wire.stepWire wire timed input
  (Right mpic, wire') <- evalRandIO $ runReaderT mb mouseX
  case mpic of
    Just pic -> return (wire', mouseX, pic)
    Nothing  -> System.exitSuccess

obtainPicture :: World -> IO Gloss.Picture
obtainPicture (_wire, _mouseX, pic) = return pic



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
batSpread = pi / 12

stateInit :: MonadRandom m => m GameState
stateInit = do
  bricks <- replicateM 10 $ do
    x <- getRandomR (screenLeftBound, screenRightBound)
    y <- getRandomR (screenLowerBound, screenUpperBound)
    r <- getRandomR (10, 50)
    return $ Brick (V2 x y) r
  return $ GameState
    { _gsBall = Ball (V2 0 0) (V2 0 (-5))
    , _gsBatX = 0
    , _gsBricks = bricks
    , _gsLastCollision = Nothing
    }

bgColor :: Gloss.Color
bgColor = Gloss.greyN 0.15

ballColor :: Gloss.Color
ballColor = Gloss.makeColorI 0x69 0x9a 0x33 0xff

batColor :: Gloss.Color
batColor = Gloss.makeColorI 0x5e 0x85 0x9a 0xff

brickColor :: Gloss.Color
brickColor = Gloss.makeColorI 0xaa 0x52 0x39 0xff
