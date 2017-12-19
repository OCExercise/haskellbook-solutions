module McCarthy91 where

mc91 :: (Integral a) => a -> a
mc91 n
  | 100 < n   = (n-10)
  | otherwise   = (mc91 . mc91) $ (n+11)
