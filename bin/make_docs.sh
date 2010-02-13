#!/bin/sh 
# This updates our documentation. :) TODO: Make it copy stuff to www dir or
# something? :)

cd /home/afp2009/instance && cabal haddock
