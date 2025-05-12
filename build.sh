#!/bin/sh

cabal clean
cabal configure
cabal build
cabal install --overwrite-policy=always
