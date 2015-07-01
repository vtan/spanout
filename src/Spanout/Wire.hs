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

import Data.Monoid



constM :: Monad m => m b -> Wire s e m a b
constM m = Wire.mkGen_ . const $ liftM Right m

bindW :: (Monad m, Monoid s) => m k -> (k -> Wire s e m a b) -> Wire s e m a b
bindW mk f = Wire.mkGen $ \s a -> do
  k <- mk
  Wire.stepWire (f k) s (Right a)

accum :: Wire.HasTime t s => (t -> b -> a -> b) -> b -> Wire s e m a b
accum f b = Wire.mkSF $ \s a ->
  let b' = f (Wire.dtime s) b a
  in  (b', accum f b')

accumE :: (b -> a -> b) -> b -> Wire s e m (Maybe a) b
accumE f b = Wire.mkSFN $ \ma ->
  case ma of
    Just a  ->
      let b' = f b a
      in  (b', accumE f b')
    Nothing -> (b, accumE f b)

switch :: (Monoid s, Monad m)
  => Wire s e m a (Either (Wire s e m a b) b) -> Wire s e m a b
switch w = Wire.mkGen $ \s a -> do
  (Right eb, w') <- Wire.stepWire w s (Right a)
  case eb of
    Left w'' -> Wire.stepWire w'' mempty (Right a)
    Right b -> return (Right b, switch w')

forThen :: (Wire.HasTime t s, Monad m) => t -> k -> Wire s e m a (Either k a)
forThen t e = proc a -> do
  t' <- Wire.time -< ()
  returnA -<
    if t' < t
    then Right a
    else Left e
