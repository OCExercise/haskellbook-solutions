module FibPattern where

fib' :: Integral a => a -> a
fib' ((-4) + 3)     = 1      -- we've subsituted (-1)
fib' (1-1)          = 1      -- we've subsituted 0
fib' ((-2) + 6 - 3) = 1      -- we've subsituted 1
fib' n              = (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))
