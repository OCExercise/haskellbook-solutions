module WordNumber where

import Data.List (intersperse)
import Data.Char (digitToInt)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Not a digit"

digits :: Int -> [Int]
digits = map digitToInt . show

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

-- Now let's use math instead

nthDigit :: (Integral a) => a -> a -> a
nthDigit x n
  | x < (10^n) = mod x 10
  | otherwise = nthDigit (div x 10) n

intToArrayOfIntsDec :: Integral a => a -> a -> [a]
intToArrayOfIntsDec n 0 = error "No int supplied"
intToArrayOfIntsDec n 1 = [nthDigit n 1]
intToArrayOfIntsDec n dec = intToArrayOfIntsDec n (dec - 1) ++ [(nthDigit n dec)]

intToArrayOfInts :: Int -> [Int]
intToArrayOfInts n = intToArrayOfIntsDec n (length . show . fromIntegral $ n)

wordNumber' :: Int -> String
wordNumber' = concat . intersperse "-" . map digitToWord . intToArrayOfInts
