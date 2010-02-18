# !/bin/sh
cabal --executable haddock && 
cabal test && 
hlint --report src/ &&
if [ -f report.html ]
then
    cp -f report.html dist/
fi
