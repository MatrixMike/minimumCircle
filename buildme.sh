#!/bin/bash
# format below as suggested by shellcheck
rm -- *.o *.hi circ circ2 new2
ghc  circ.hs
ghc circ2.hs
ghc  new2.hs

