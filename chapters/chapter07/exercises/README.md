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
