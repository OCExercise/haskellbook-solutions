module AddFive where

-- Pop quiz: using 5 as a literal in an expression will constrain addFive to
-- Integer -> Integer -> Integer. Why is that?
c :: Num a => a
c = 5

-- Pop quiz: why does type class Num not derive Ord?
-- hint: complex numbers are not orderable
addFive :: (Num a, Ord a) => a -> a -> a
addFive = (\x -> (\y -> (if x > y then y else x) + c))
