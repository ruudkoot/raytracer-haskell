#!/bin/sh 
# Script that you can run before you push your stuff.


cd .. &&
cabal install &&
cabal --executable haddock &&
cabal test
