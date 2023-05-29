# spanout [![Hackage](https://img.shields.io/hackage/v/spanout.svg)](https://hackage.haskell.org/package/spanout)

A breakout clone written in Haskell using the
FRP library [*netwire*](http://hackage.haskell.org/package/netwire) and
vector graphics library [*gloss*](https://hackage.haskell.org/package/gloss).

![Screenshot](screenshot.png?raw=true)

## Usage

Hit all the bricks and don't let the ball fall off.

* Mouse – bat control
* `Space` – skip current level

## Compiling

Tested with GHC 9.2.8 and Cabal 3.6.2.0, both of which you can install with ghcup:
```
cabal run
```
