module DivisionIntegral where

div' :: (Integral a) => a -> a -> (a,a)
div' n d = div'' n d 0
  where div'' n' d' count
          | n' < d' = (count, n')
          | otherwise = div'' (n' - d') d' (count + 1)
