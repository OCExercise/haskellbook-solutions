module Main where

rvrs :: String -> String
rvrs x = reverse x

main :: IO ()
main = do
    putStrLn (reverse "test")
