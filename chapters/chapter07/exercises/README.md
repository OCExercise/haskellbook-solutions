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
