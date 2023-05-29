{-# LANGUAGE Arrows #-}

module Spanout.Wire
  ( constM
  , bindW
  , accum
  , accumE
  , switch
  , forThen

  , Wire
  , Wire.Timed(..)
  , Wire.stepWire
  , Wire.delay
  , Wire.time
  ) where

import Control.Arrow
import Control.Monad
import Control.Wire (Wire)
import qualified Control.Wire as Wire

-- A reactive value from a monadic value
constM :: Monad m => m b -> Wire s e m a b
constM m = Wire.mkGen_ . const $ liftM Right m

-- Peforms a monadic action and parametrizes a wire with the resulting value
bindW :: (Monad m, Monoid s) => m k -> (k -> Wire s e m a b) -> Wire s e m a b
bindW mk f = Wire.mkGen $ \s a -> do
  k <- mk
  Wire.stepWire (f k) s (Right a)

-- A wire that yields an accumulated value based on its input and a time delta
accum :: Wire.HasTime t s => (t -> b -> a -> b) -> b -> Wire s e m a b
accum f b = Wire.mkSF $ \s a ->
  let b' = f (Wire.dtime s) b a
  in  (b', accum f b')

-- A wire that yields an accumulated value based on events
accumE :: (b -> a -> b) -> b -> Wire s e m (Maybe a) b
accumE f b = Wire.mkSFN $ \ma ->
  case ma of
    Just a  ->
      let b' = f b a
      in  (b', accumE f b')
    Nothing -> (b, accumE f b)

-- Switches from a wire that produces either an output or a new wire
switch :: (Monoid s, Monad m)
  => Wire s e m a (Either (Wire s e m a b) b) -> Wire s e m a b
switch w = Wire.mkGen $ \s a -> do
  (eb, w') <- Wire.stepWire w s (Right a)
  case eb of
    Right (Left w'') -> Wire.stepWire w'' mempty (Right a)
    Right (Right b)  -> return (Right b, switch w')
    Left e           -> return (Left e, switch w')

-- Acts as the identity wire for given time, then yield a constant value
forThen :: (Wire.HasTime t s, Monad m) => t -> k -> Wire s e m a (Either k a)
forThen t e = proc a -> do
  t' <- Wire.time -< ()
  returnA -<
    if t' < t
    then Right a
    else Left e
