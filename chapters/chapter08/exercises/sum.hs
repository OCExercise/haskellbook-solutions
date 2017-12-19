module Sum where

import Data.Maybe (fromJust)

sum' :: (Integral a) => a -> Maybe a
sum' n
    | n < 1     = Nothing
    | n == 1    = Just 1
    | otherwise = Just $ (+n) . fromJust $ sum'(n - 1)
