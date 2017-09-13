{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheTypeProblem2 where

x = 5
y = x + 5
w = y * 10

x = 5
y = x + 5
z y = y * 10
