# spanout

A breakout clone written in Haskell using the FRP library
[*netwire*](http://hackage.haskell.org/package/netwire) and vector graphics library
[*gloss*](https://hackage.haskell.org/package/gloss).

## Usage

Hit all the bricks and don't let the ball fall off.

* Mouse – bat control
* `Space` – skip current level

## Compiling

You need GHC and Cabal in order to compile *spanout*.
Cabal 1.18+ and an updated package list (`cabal update`) are recommended.

Building with local package installs only:
~~~
git clone https://github.com/vtan/spanout.git
cd spanout
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
cabal run
~~~

Building without a sandbox – pollutes global or user package database:
~~~
git clone https://github.com/vtan/spanout.git
cd spanout
cabal install --only-dependencies
cabal configure
cabal build
cabal run
~~~
