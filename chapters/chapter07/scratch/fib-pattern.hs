module FibPattern where

fib' :: Integral a => a -> a
fib' (-1) = 1
fib' 0    = 1
fib' 1    = 1
fib' n    = (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))
