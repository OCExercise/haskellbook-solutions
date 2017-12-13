# Exercise Solutions

## Directions

* [Table of Contents](../../../README.md)
* Review the [notes here](../README.md).

## Grab Bag

1. Which (two or more) of the following are equivalent?
    * Options:
        1. `mTh x y z = x * y * z`
        1. `mTh' x y = \z -> x * y * z`
        1. `mTh'' x = \y -> \z -> x * y * z`
        1. `mTh''' = \x -> \y -> \z -> x * y * z`
    * `1`, `2`, `3` and `4`.
1. The type of `mTh` is `Num a => a -> a -> a -> a`. What is the type of `mTh''`?
    * `c) Num a => a -> a -> a -> a`
1. Rewrite the following functions as either lambda expressions or unfurl lambda expressions into singularly bound named expressions according to the instructions.
    1. Rewrite the `f` function in the where clause ([addOneIfOdd.hs](addOneIfOdd.hs)).
        ```haskell
        -- Original
        addOneIfOdd n case odd n of
            True -> f n
            False -> n
            where f n = n + 1

        -- Lambda form in f
        addOneIfOdd n case odd n of
            True -> f n
            False -> n
            where f = \n -> n + 1
        ```
    1. Rewrite the whole thing ([addFive.hs](addFive.hs)).
        ```haskell
        -- Original
        addFive x y = (if x > y then y else x) + 5

        -- Lambda form
        c :: Num a => a
        c = 5

        addFive :: (Num a, Ord a) => a -> a -> a
        addFive = (\x -> (\y -> (if x > y then y else x) + c))

        ```
    1. Unfurl lambda expression (trivial, not going to break out into a separate file).
        ```haskell
        mFlip f x y = f x y
        ```

## Variety Pack

1. Answer some questions about the following set of expressions.
    ```haskell
    k (x, y) = x
    k1 = k ((4 - 1), 10)
    k2 = k ("three", (1 + 2))
    k3 = k (3, True)
    ```
    1. What is the type of `k`?
        * `k :: (a, b) -> a`.
    1. What is the type of `k2`? Is it the same type as `k1` or `k3`
        * `k :: [Char]`. No. The types for `k1` and `k3` are `Num a => a`.
    1. Of `k1`, `k2` and `k3`, which will return a numeric 3?
        * `k1` and `k3`.
1. Fill in the definition for the function
    ```haskell
    f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
    f = undefined

    -- Answer
    f (a, b, c) (d, e, f) = ((a,d), (c,f))
    ```

## Case Practice

Rewrite the following functions to use case expression syntax.

1. Rewrite the following functions to use case expression syntax
    * `functionC x y = if (x > y) then x else y`
        ```haskell
        functionC :: Ord a => a -> a -> a
        functionC x y =
            case (x <= y) of         -- what can I say? I prefer least to greatest.
                False -> x           -- note (x > y) is open when (x == y) in the
                _ -> y               -- original
        ```
1. Do the same for this function.
    * `ifEvenAdd2 n = if even n then (n + 2) else n`
        ```haskell
        ifEvenAdd2 :: Integral a => a -> a
        ifEvenAdd2 n =
            case even n of
                True -> n + 2
                False -> n
        ```
1. Fix the following function such that the case statement is fully exhaustive
    ```haskell
    nums x =
        case compare x 0 of
            LT -> -1
            GT -> 1
    ```
    * Broken in the case where `x` is bound to 0. `compare` would yield `EQ` as a result, which is the missing case. The fix:
    ```haskell
    nums x =
        case compare x 0 of
            LT -> -1
            GT -> 1
            EQ -> 0
    ```

## Guard Duty

1. Rewrite `avgGrade` and insert `otherwise = 'F'` above the patterns. The original is available [here](avgGrade.hs)
    ```haskell
    module AvgGrade where

    avgGrade :: (Fractional a, Ord a) => a -> Char
    avgGrade x
      | otherwise = 'F'
      | y >= 0.9  = 'A'
      | y >= 0.8  = 'B'
      | y >= 0.7  = 'C'
      | y >= 0.59 = 'D'
      | y <  0.59 = 'F'
      where
        y = x / 100
    ```
    - What is the result of `f 90`? `f 75`? `f 60`?
        - `'F'` is returned for all inputs to `f`.
1. Take the original implementation of `avgGrade` an reorder some of the guard clauses.
    ```haskell
    module AvgGrade where

    avgGrade :: (Fractional a, Ord a) => a -> Char
    avgGrade x
      | y >= 0.7  = 'C'
      | y >= 0.9  = 'A'
      | y >= 0.8  = 'B'
      | y >= 0.59 = 'D'
      | y <  0.59 = 'F'
      where
        y = x / 100

    ```
    - Does the result still type check?
        - Yes
    - Is the resulting function equivalent to the original implementation?
        - No. Guards are evaluated from top down. The condition for achieving, say, a **C** is also satisfied for the condition of achieving an **A**. As `y >= 0.7` is satisfied first, the result for `f 90` will be `'C'`
1. Multiple choice. Given an implementation of a palindrome, complete the question "[t]he following function will return..."
    ```haskell
    pal xs
        | xs == reverse xs  = True
        | otherwise         = False
    ```
    - Options:
        a. `xs` written backwards when it's `True`
        b. True when `xs` is a palindrome
        c. `False` when `xs` is a palindrome
        d. `False` when `xs` is reversed
    - Answer: `b`
1. What types of arguments can the function above (`pal`) take?
    - Anything in a list (`[a]`)
1. What is the type of `pal`?
    - `pal :: [a] -> Bool`
1. Multiple choice. The following function returns:
    ```haskell
    numbers x
        | x < 0     = -1
        | x == 0    = 0
        | x > 0     = 1

    ```
    - Options:
        a. the value of its argument plus or minus 1
        b. the negation of its argument
        c. an indication of whether its argument is a positive or negative number or zero
        d. binary machine language
    - Answer: `c`
1. What types of arguments can the function above (`numbers`) take?
    - `(Num a, Ord a) => a`
1. What is the type of `numbers`
    - `numbers :: (Num a, Ord a, Num b) => a -> b`

## Chapter Exercises

### Multiple Choice

1. A polymorphic function:
    - `d`: may resolve to values of different types depending on the input
1. Two functions named f and g have types `Char` -> `String` and `String` -> `[String]` respectively. The composed function `g . f` has the type
    - `b`: `Char -> [String]`
1. A function `f` has the type `Ord a => a -> a -> Bool` and we apply it to one numeric value. What is the type now?
    - `a`: `Ord a => a -> Bool`
1. A function with the type `(a -> b) -> c`
    - `b`: is a higher order function.
1. Given the definition of `f` below, which of the following expression will not result in an error?
    ```haskell
    f :: a -> a
    f x = x
    ```
    - `a`: `f True :: Bool`

## Let's write code

1. Given code that extracts the tens digit:
    ```haskell
    tensDigit :: Integral a => a -> a
    tensDigit x = d
        where   xLast   = x `div` 10
                d       = xLast `mod` 10
    ```
    a. rewrite using `divMod`
        ```haskell
        tensDigit :: Integral a => a -> a
        tensDigit x
            | (-10) < x && x < 10 = error "No tenth digit"
            | otherwise = mod (fst (divMod (abs(x)) 10)) 10
        ```
    b. Does the divMod version have the same type as the original version?
        - Yes.
    c. Define a function that returns the hundreds digit
        ```haskell
        hundredsDigit :: Integral a => a -> a
        hundredsDigit x
            | (-10) < x && x < 10 = error "No hundredth digit"
            | otherwise = mod (fst (divMod (abs(x)) 100)) 10
        ```
2. Implement variations of `foldBool` using case expressions and guards.
    ```haskell
    foldBool :: a -> a -> Bool -> a
    foldBool = error "Unimplemented"
    ```
    A pattern matching example is supplied by the text
    ```haskell
    foldBool :: a -> a -> Bool -> a
    foldBool x _ True = x
    foldBool _ y False = y
    ```
    * With a case expression
        ```haskell
        foldBool :: a -> a -> Bool -> a
        foldBool x y z =
            case z of
                True <- x
                False <- y
        ```
    * With a guard
        ```haskell
        foldBool :: a -> a -> Bool -> a
        foldBool x y z
            | z == True     = x
            | otherwise     = y
        ```
3. Implement `g` given the following type signature
    ```haskell
    g :: (a -> b) -> (a,c) -> (b,c)
    g = undefined
    ```
    * This is a higher order function
        ```haskell
        g :: (a -> b) -> (a,c) -> (b,c)
        g f (a,c) = (f a, c)
        ```
4. Examine [arith4.hs](arith4.hs) and comment
    ```haskell
    module Arith4 where

    roundTrip :: (Show a, Read a) => a -> a
    roundTrip a = read (show a)

    main :: IO ()
    main = do
      print (roundTrip 4)
      print (id 4)
    ```
    - `roundTrip` implements an identity function. This is obvious from inspection of the type and recognizing that `a -> a` can only be satisfied by a function that ultimately returns its input.
        ```haskell
        ghci> roundTrip 4 == id 4
        True
        ```
5. Write a pointfree version of `round trip` ( [arith4-pointfree.hs](arith4-pointfree.hs)):
    ```haskell
    module Arith4PointFree where

    roundTrip :: (Show a, Read a) => a -> a
    roundTrip = read . show

    main :: IO ()
    main = do
      print (roundTrip 4)
      print (id 4)
    ```
