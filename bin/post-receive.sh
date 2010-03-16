#!/bin/sh 

title() {
  echo "============================================"
  echo "  $1"
  echo "============================================"
}


# Update PATH so that cabal can find the right executables
PATH="/home/afp2009/.cabal/bin:$PATH"

title "Updating Instance" &&
./update_instance.sh 2>/dev/null && 


title "Updating RedMine Clone" &&
./update_redmine_mirror.sh >/dev/null && 

# Build programme
title "Building Program" &&
cd .. && 
cabal install --enable-executable-profiling && 

# Run tests (configure with UserHooks)
title "Running Tests [TODO]" &&

# Generate Haddock documentation
title "Generating Haddock Documentation" &&
cabal --executable haddock &&

# Copy website
title "Copying www/ Folder to Website" &&
cp -r www/* /var/www/projects/afp2009/ &&

# Copy generated docs
title "Copying generating Haddock docs" &&
cp -r dist/doc/html/RayTracer/raytrace/* /var/www/projects/afp2009/doc/ &&
echo "  Browse haddock documentation at: http://projects.spockz.nl/projects/afp2009/doc/" && 

# Run Hlint on all haskell source files
title "Linting" && 
find -name "*.hs" | xargs hlint 

