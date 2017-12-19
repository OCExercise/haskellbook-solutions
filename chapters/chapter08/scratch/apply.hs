module Apply where

apply :: (Eq a, Num a) => a -> (b -> b) -> b -> b
apply 0 f = id
apply n f = f . apply (n - 1) f 
