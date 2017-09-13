{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheTypeProblem1 where -- simple example

example = 1

a = (* 9) 6
b = head [(0,"doge"),(1,"kitteh")]
c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
d = if False then True else False
e = length [1, 2, 3, 4, 5]
f = (length [1, 2, 3, 4]) > (length "TACOCAT")
