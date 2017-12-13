module Arith2 where

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> Int -> Int
add = (+)

inc :: Int -> Int
inc = \x -> x + 1

inc' :: Int -> Int
inc' = (+1)

main :: IO ()
main = do
  print (0::Int)
  print (add 1 0)
  print (inc 0)
  print (inc' 0)
  print ((inc . inc) 0)
  print ((inc' . inc) 0)
  print ((inc . inc') 0)
  print (negate (inc 0))
  print ((negate . inc) 0)
  print ((inc . inc . inc . negate . inc) 0)
