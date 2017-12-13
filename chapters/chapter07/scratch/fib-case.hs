module FibCase where

fib' :: Integral a => a -> a
fib' n =
  case (-1 <= n && n <= 1) of
    True  -> 1
    False -> (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))

-- use the wildcard (_) pattern to match the exhaustive cases
fib'' :: Integral a => a -> a
fib'' n =
  case n of
    -1  -> 1
    0  -> 1
    1  -> 1
    _ -> (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))
