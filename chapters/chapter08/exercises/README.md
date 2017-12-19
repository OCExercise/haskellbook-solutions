# Exercise Solutions

## Directions

* [Table of Contents](../../../README.md)
* Review the [notes here](../README.md).

## Intermission: Exercise

Evaluate the following:
```haskell

apply :: (Eq a, Num a) => a -> (b -> b) -> b -> b
apply 0 f = id
apply n f = f . apply (n - 1) f

apply 5 (+1) 5
```
* **Answer**: `10`

## Chapter Exercises

### Review the types

1. What is the type of `[[True, False], [True, True], [False, True]]`?
    - **Answer**: `[[Bool]]`
1. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`?
    - Options:
        a. `[(True, False), (True, True), (False, True)]`
        b. `[[3 == 3], [6 > 5], [3 < 4]]`
        c. `[3 == 3, 6 > 5, 3 < 4]`
        d. `["Bool", "more Bool", "Booly Bool!"]`
    - **Answer**: `b) [[3 == 3], [6 > 5], [3 < 4]]`
1. Which is true about the following function?
    ```haskell
    func :: [a] -> [a] -> [a]
    func x y = x ++ y
    ```
    - Options:
        a. `x` and `y` must both be of the same type
        b. `x` and `y` must both be lists
        c. if `x` is `String` then y must be `String`
        d. all of the above
    - **Answer**: `d)` All of the above.
1. Which of the following is a valid application of `func` defined above to its arguments?
    - Options:
        a. `func "Hello World"`
        b. `func "Hello" " World"`
        c. `func [1,2,3] "a, b, c"`
        d. `func ["Hello", " World"]`
    - **Answer:**: `b) func "Hello" " World"`

### Reviewing currying

We're provided the following definitions:
    ```haskell
    cattyCon :: String -> String -> String
    cattyCon x y = x ++ " mrow " ++ y

    flippy :: String -> String -> String


    appedCatty :: String -> String
    appedCatty = cattyCon "woops"

    frappe :: String -> String
    frappe = flippy "haha"
    ```
    1. What is the value of `appedCatty "woohoo"`?
        - **Answer**: `"woops mrow woohoo"`
    1. What is the value of `frappe "1"`?
        - **Answer**: `"1 mrow haha"`
    1. What is the value of `frappe (appedCatty "2")`?
        - **Answer**: `"woops mrow 2 mrow haha"`
    1. What is the value of `appedCatty (frappe "blue")`"?
        - **Answer**: `"woops mrow blue mrow haha"`
    1. What is the value of the following?
        ```haskell
        cattyCon (frappe "pink") (cattyCon "green" (appedCatty "blue"))
        ```
        - **Answer**: `"pink mrow haha mrow green mrow woops mrow blue"`
    1. What is the value of `cattyCon (flippy "Pugs" "are") "awesome"`?
        - **Answer**: `"are mrow Pugs mrow awesome"`

### Recursion

1. Write out the steps of `div' 15 2` using the definition of `div'` prepared in the notes:
    ```haskell
    15 `div'` 2
    -- 15 - 2, 13, count == 1
    --      2, 11, count == 2
    --      2, 9, count == 3
    --      2, 7, count == 4
    --      2, 5, count == 5
    --      2, 3, count == 6
    --      2, 1, count == 7
    (7,1)
    ```
1. Write a function that recursively sums all numbers from 1 to n (see [sum.hs](sum.hs)).
    ```haskell
    sum' :: (Integral a) => a -> Maybe a
    sum' n
        | n < 1     = Nothing
        | n == 1    = Just 1
        | otherwise = Just $ (+n) . fromJust $ sum'(n - 1)a

    -- ...
    ghci> :l exercises/sum.hs
    [1 of 1] Compiling Sum              ( exercises/sum.hs, interpreted )
    Ok, modules loaded: Sum.
    ghci> sum' 10
    Just 55
    ```
1. Implement binary multiplication via recursive summation (see [mult.hs](mult.hs))
    ```haskell
    module Mult where

    mult :: (Integral a) => a -> a -> a
    mult x y = sign * mult' x y 1
      where mult' x' y' count
              | count == abs(y')  = x'
              | otherwise         = x' + (mult' x' y' (count + 1))
            sign = y `div` abs(y)

    -- ...
    ghci> :l exercises/mult.hs
    [1 of 1] Compiling Mult             ( exercises/mult.hs, interpreted )
    Ok, modules loaded: Mult.
    ghci> mult (2) (-10)
    -20
    ghci> mult (2) (10)
    20
    ```

### Fixing dividedBy

Fix implemented in `div'` such that:
1. Division by zero returns some graceful value.
1. Negative integer division is fully supported.
    ```haskell
    module DivisionIntegralFixed where

    data QuotRem a = Result a | DivisionByZero deriving (Show)

    fromResult :: QuotRem a -> a
    fromResult (Result a) = a

    div' :: (Integral a) => a -> a -> QuotRem (a, a)
    div' n 0 = DivisionByZero
    div' n d = div'' n d 0
      where div'' n' d' count
              | abs(n') < abs(d') = Result ((s n) * (s d) * count, (s n) * (s d) * n')
              | otherwise = Result $ fromResult $ div'' (abs(n') - abs(d')) (abs(d')) (count + 1)
            s y =
                case y < 0 of
                  True -> (-1)
                  False -> 1
    -- ...

    ghci> :l exercises/division-integral-fixed.hs
    [1 of 1] Compiling DivisionIntegralFixed ( exercises/division-integral-fixed.hs, interpreted )
    Ok, modules loaded: DivisionIntegralFixed.
    ghci> div' 10 (0)
    DivisionByZero
    ghci> div' 10 (3)
    Result (3,1)
    ghci> div' 10 (-3)
    Result (-3,-1)
    ghci> div' (-10) (-3)
    Result (3,1)
    ghci> div' (-10) (3)
    Result (-3,-1)
    ```
    - Our solution is implemented in [division-integral-fixed.hs](exercises/division-integral-fixed.hs)

### McCarthy 91 function

[McCarthy 91](https://en.wikipedia.org/wiki/McCarthy_91_function) is a [formal verification](https://en.wikipedia.org/wiki/Formal_verification) function defined by computer scientist [John McCarthy](https://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)). Let's try implementing it. We'll be making use of [`map`](https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:map) on this bombing run (see [mc91.hs](exercises/mc91.hs)).
    ```haskell

    module McCarthy91 where

    mc91 :: (Integral a) => a -> a
    mc91 n
      | 100 < n   = (n-10)
      | otherwise   = (mc91 . mc91) $ (n+11)

    -- ...
    ghci> :l exercises/mc91.hs
    [1 of 1] Compiling McCarthy91       ( exercises/mc91.hs, interpreted )
    Ok, modules loaded: McCarthy91.
    ghci> map mc91 [95..110]
    [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
    ```

### Numbers into words

Implement the following (see [wordnumber.hs](exercises/wordnumber.hs)):
    ```haskell
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
    ```
