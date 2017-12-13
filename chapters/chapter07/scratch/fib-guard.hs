module FibGuard where

fib' :: Integral a => a -> a
fib' n
  | -1 <= n && n <= 1   = 1
  | otherwise           = (n `quot` abs(n)) * (fib' ((abs(n))-1) + fib' (abs(n)-2))


-- a bit contrived in the second and exhaustive conditions
-- to illustrate a three condition guard
fib'' :: Integral a => a -> a
fib'' n
  | -1 <= n && n <= 1   = 1
  | 1 < n               = (fib' (n-1) + fib' (n-2)) -- positive only Fibonacci
  | otherwise           = (-1) * (fib' ((abs(n))-1) + fib' (abs(n)-2))
