module DivisionIntegralFixed where

data QuotRem a = Result a | DivisionByZero deriving (Show)

fromResult :: QuotRem a -> a
fromResult (Result a) = a

div' :: (Integral a) => a -> a -> QuotRem (a, a)
div' n 0 = DivisionByZero
div' n d = div'' n d 0
  where div'' n' d' count
          | abs(n') < abs(d') = Result ((s n) * (s d) * count, (s n) * (s d) * n')
          | otherwise = Result $ fromResult $ div'' (abs(n') - abs(d')) (abs(d')) (count + 1)
        s y =
            case y < 0 of
              True -> (-1)
              False -> 1
