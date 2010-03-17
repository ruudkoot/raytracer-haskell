# !/bin/sh
cabal configure --enable-executable-profiling
cabal build
dist/build/raytrace/raytrace +RTS -P -hc -RTS $*