module FoldExercises where

sum' :: (Num a, Foldable t) => t a -> a
sum' xs = foldl (\y x -> y + x) 0 xs

product' :: (Num a, Foldable t) => t a -> a
product' xs = foldl (\y x -> y * x) 1 xs

difference :: (Num a, Foldable t) => t a -> a
difference xs = foldl (\y x -> y - x) 0 xs
