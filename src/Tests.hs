module Main where

import qualified Spanout.Level

import qualified Test.Framework as Test



main :: IO ()
main = Test.defaultMain
  [ Spanout.Level.test
  ]
