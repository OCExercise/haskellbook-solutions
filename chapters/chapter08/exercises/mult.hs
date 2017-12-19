module Mult where

mult :: (Integral a) => a -> a -> a
mult x y = sign * mult' x y 1
  where mult' x' y' count
          | count == abs(y')  = x'
          | otherwise         = x' + (mult' x' y' (count + 1))
        sign =
          case y < 0 of
            True <- (-1)
            False <- 1
