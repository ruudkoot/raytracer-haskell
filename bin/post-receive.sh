#!/bin/sh 


./update_instance.sh && 
./update_redmine_mirror.sh && 
cd .. && 
cabal install && 
cabal test && 
cabal --executable haddock &&
find -name "*.hs" | xargs hlint

# cp generated docs
