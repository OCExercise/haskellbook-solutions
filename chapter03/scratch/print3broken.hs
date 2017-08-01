module Mail where

printSecond :: String -> IO ()
printSecond greeting = do
    putStrLn greeting

main :: IO () 
main = do
    putStrLn greeting
    printSecond greeting
    where greeting = "Yarrrr"
