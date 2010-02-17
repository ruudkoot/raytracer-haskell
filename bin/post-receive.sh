#!/bin/sh 

# Update PATH so that cabal can find the right executables
PATH="/home/afp2009/.cabal/bin:$PATH"

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

# Copy website
cp -r www/* /var/www/projects/afp2009/

# Copy generated docs TODO
cp -r dist/doc/html/RayTracer/raytrace/* /var/www/projects/afp2009/doc/

# Run Hlint on all haskell source files
find -name "*.hs" | xargs hlint

