{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

module Spanout.Wire
  ( (<+>)
  , (<+|)
  , constM
  , delayM
  , bindW
  , overW
  , overI
  , WireTag(..)
  , (=>>>)
  , switch
  , followedBy
  , choose

  , Wire
  , Wire.Timed(..)
  , Wire.stepWire
  , Wire.delay
  , Wire.time
  ) where

import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Lens
import Control.Monad (liftM)
import Control.Wire (Wire)
import qualified Control.Wire as Wire

import Data.Maybe (fromMaybe)
import Data.Monoid



infixr 5 <+>
(<+>) :: Monad m
  => Wire s e m a (Maybe b) -> Wire s e m a (Maybe b) -> Wire s e m a (Maybe b)
(<+>) = liftA2 (<|>)

infix 4 <+|
(<+|) :: Monad m
  => Wire s e m a (Maybe b) -> Wire s e m a b -> Wire s e m a b
(<+|) = liftA2 $ flip fromMaybe

constM :: Monad m => m b -> Wire s e m a b
constM m = Wire.mkGen_ . const $ liftM Right m

delayM :: Monad m => m a -> Wire s e m a a
delayM ma = Wire.mkGenN $ \a' -> do
  a <- ma
  return (Right a, Wire.delay a')

bindW :: (Monad m, Monoid s) => m k -> (k -> Wire s e m a b) -> Wire s e m a b
bindW mk f = Wire.mkGen $ \s a -> do
  k <- mk
  Wire.stepWire (f k) s (Right a)

overW :: Arrow p => Lens s s a a -> p a a -> p s s
overW l w = proc a -> do
  s <- w -< view l a
  returnA -< set l s a

overI :: Arrow p => Lens s s a a -> p a (Maybe a) -> p s (Maybe s)
overI l w = proc a -> do
  ms <- w -< view l a
  returnA -< fmap (\s -> set l s a) ms

class WireTag t where
  infixr 1 =>>>=
  (=>>>=) :: ArrowChoice p => p a (t b) -> p b (t c) -> p a (t c)

instance WireTag Maybe where
  p1 =>>>= p2 = proc a -> do
    mb <- p1 -< a
    case mb of
      Just b  -> p2 -< b
      Nothing -> returnA -< Nothing

instance WireTag (Either e) where
  p1 =>>>= p2 = proc a -> do
    eb <- p1 -< a
    case eb of
      Right b -> p2 -< b
      Left  e -> returnA -< Left e

infixr 1 =>>>
(=>>>) :: (Applicative t, WireTag t, ArrowChoice p)
  => p a (t b) -> p b c -> p a (t c)
p1 =>>> p2 = p1 =>>>= (p2 >>^ pure)

switch :: (Monad m, Monoid s)
  => Wire s e m a (Either (Wire s e m a b) b)
  -> Wire s e m a b
switch w = Wire.mkGen $ \s a -> do
  (Right eb, w') <- Wire.stepWire w s (Right a)
  case eb of
    Left  we -> Wire.stepWire we mempty (Right a)
    Right b  -> return (Right b, switch w')

followedBy :: (Monad m, Monoid s, Applicative t)
  => Wire s e m a (Maybe b)
  -> Wire s e m a (t b)
  -> Wire s e m a (Either (Wire s e m a (t b)) (t b))
w1 `followedBy` w2 = w1 >>> arr f
  where
    f (Just b) = Right $ pure b
    f Nothing  = Left w2

choose :: (Monad m, Monoid s, Applicative t)
  => Wire s e m a (Either k b)
  -> (k -> Wire s e m a (t b))
  -> Wire s e m a (Either (Wire s e m a (t b)) (t b))
w1 `choose` g = w1 >>> arr f
  where
    f (Right b) = Right $ pure b
    f (Left  k) = Left  $ g k
