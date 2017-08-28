-- exercises/top_or_local.hs
module TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x   = x + woot + topLevelValue
          where woot :: Integer
                woot = 10

topLevelValue :: Integer
topLevelValue = 5
