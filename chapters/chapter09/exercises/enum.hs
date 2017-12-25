module Enum where

enumFromTo' :: (Enum a) => a -> a -> [a]
enumFromTo x y
  | x == y  = x:[]
  | x < y
