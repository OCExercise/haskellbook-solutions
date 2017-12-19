module FactorialMaybe where

import Data.Maybe (fromJust)

factorial :: (Integral a) => a -> Maybe a
factorial n
    | n < 0     = Nothing
    | n == 0    = Just 1
    | otherwise = Just $ (*n) . fromJust . factorial $ (n-1)
