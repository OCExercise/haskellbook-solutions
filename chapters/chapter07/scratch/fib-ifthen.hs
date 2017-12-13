module FibIfThen where

fib' :: Integral a => a -> a
fib' n =
    if (n == 0)
        then 1
        else if (n == 1)
            then 1
            else if (n == (-1))
              then (-1)
              else (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))
