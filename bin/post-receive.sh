#!/bin/sh 


# Update instance
./update_instance.sh && 

# Update redmine clone 
./update_redmine_mirror.sh && 

# Build programme
cd .. && 
cabal install && 

# Run tests (configure with UserHooks)
cabal test && 

# Generate Haddock documentation
cabal --executable haddock &&

# Copy generated docs TODO

# Run Hlint on all haskell source files
find -name "*.hs" | xargs ~/.cabal/bin/hlint

