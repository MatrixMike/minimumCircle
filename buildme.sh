#!/bin/bash
rm *.o *.hi circ circ2
ghc  circ.hs
ghc circ2.hs

