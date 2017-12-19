# Recursion

## Directions

* [Table of Contents](../../README.md)
* Solutions availables [here](exercises/README.md).

## Introduction

Finally we're on **recursion**, or as I like to call it "doing factorial the hard way." (we saw two constant time implementations [in the last chapter](../chapters07/README.md)).

* **Recursion** is self-referential invocation of a function. That is a function calls itself *from within* itself.
* In [lambda calculus](../chapter01/README.md#Combinators), we implement recursion through the use of a particular [fixed point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator)  [Y](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus).
    - As it stands, [we can't actually implement Y combinator](http://blog.ezyang.com/2012/09/y-combinator-and-strict-positivity/) in a straight forward fashion in Haskell. But it's good to get a look at the error and try and make sense of it.
        ```haskell
        ghci> :t (\f -> (\x -> f(x x))(\x -> f( x x)))


        <interactive>:1:19: error:
            • Occurs check: cannot construct the infinite type: t0 ~ t0 -> t
              Expected type: t0 -> t
                Actual type: (t0 -> t) -> t
            • In the first argument of ‘x’, namely ‘x’
              In the first argument of ‘f’, namely ‘(x x)’
              In the expression: f (x x)
            • Relevant bindings include
                x :: (t0 -> t) -> t (bound at <interactive>:1:10)
                f :: t -> t (bound at <interactive>:1:3)

        <interactive>:1:34: error:
            • Occurs check: cannot construct the infinite type: t0 ~ t0 -> t
            • In the first argument of ‘x’, namely ‘x’
              In the first argument of ‘f’, namely ‘(x x)’
              In the expression: f (x x)
            • Relevant bindings include
                x :: t0 -> t (bound at <interactive>:1:24)
                f :: t -> t (bound at <interactive>:1:3)


        ```
        - On the down side, there *is* an error. The silver lining is the message `cannot construct the infinite type: t0 ~ t0 -> t` indicates that this particular combinator *is* recursive.
* In Haskell as in other practical programming languages, we can bind functions to names and vastly simplify implementation of recursion.

## Factorial!

Implementing the [factorial operation](https://en.wikipedia.org/wiki/Factorial) is a useful way to explore recursion. Our objectives here are to:
1. Distinguish between *terminating* and *non-terminating* recursive execution.
2. Deploy function composition and higher order functions
3. Recover the Y combinator from that nasty error above

* First, let's examine the canonical, single function implementation of factorial (using [guards](../chapters07/README.md#guards)):
    ```haskell
    factorial n
      | n < 0 = error "n must be non-negative"
      | n == 0 = 1
      | otherwise = n * factorial(n-1)
    ```
    This function
    - errors on anything other than non-negative input,
    - terminates quickly once its argument matches 0, and
    - recurses for any other input.
    - `n < 0` and `n == 0` are **base cases**. Recursion ceases once either of those conditions are meant. `n < 0` is also an error case. Factorial [is not defined](https://en.wikipedia.org/wiki/Factorial#Non-extendability_to_negative_integers) for negative integers.
* Remove the **base cases** of the above function would result in non-terminating recursion
    ```haskell
    ghci> :r
    ghci> factorial n = n * factorial(n - 1) -- removed guards since we
                                             -- have but a single case
    ghci> factorial (-1)
    ^CInterrupted. -- just hangs
    ghci> factorial 6
    ^CInterrupted. -- just hangs
    ```

### Another way to look at recursion

* Recursion can be thought of as self-referential function composition. That is the result of a single application of a function to is input is applied to the *same* function repeatedly until a base case is matched and the chain terminates (failure to every converge on a base case results in non-terminating behavior).
* The following function `inc` is applied three times naively.
    ```haskell
    ghci> :{
    Prelude| inc :: Num a => a -> a
    Prelude| inc = (+1)
    Prelude| :}
    ghci> three = inc . inc . inc
    ghci> three 0
    3
    ```
* Let's refactor this to make explicit a compositional formulation of recursion (see [apply.hs](scratch/apply.hs)).
    ```haskell
    module Apply where

    apply :: (Eq a, Num a) => a -> (b -> b) -> b -> b
    -- point free
    apply 0 g = id
    apply n f = f . apply (n - 1) f
    ```
    - `f` is composed with `apply` which takes a decremented count and `f` as arguments.

## Bottom

* **[Bottom](https://wiki.haskell.org/Bottom)** (symbol: `⊥`) computations never complete successfully, including computions which exit due to error. The following example differs from the text as of GHC 8.0.2:
    ```haskell
    -- GHCi does not report a loop exception
    ghci> x = x in x
    ^CInterrupted.

    -- GHCi reports non-exhaustive pattern error for True, exception for False
    ghci> f True = 0
    ghci> f False = error "blah"
    ghci> f True
    0
    ghci> f True
    *** Exception: <interactive>:31:1-22: Non-exhaustive patterns in function f
    ghci> f False
    *** Exception: blah
    CallStack (from HasCallStack):
      error, called at <interactive>:31:11 in interactive:Ghci8

    ghci> :r -- clear
    Ok, modules loaded: Apply.

    -- Same thing happens if we apply a wildcard catchall
    ghci> f True = 0
    ghci> f _ = error "blah"
    ghci> f True
    *** Exception: blah
    CallStack (from HasCallStack):
    error, called at <interactive>:51:7 in interactive:Ghci2

    ghci> :r -- clear
    Ok, modules loaded: Apply.

    -- f True now returns 0, f False returns non-exhaustive pattern error
    ghci> f True = 0
    ghci> f True
    0
    ghci> f False
    *** Exception: <interactive>:46:1-10: Non-exhaustive patterns in function f
    ```
* Bottom functions like `f` are **partially** implemented.  We want **totally** implemented functions. To that end, let's introduce ourselves to the [Maybe](https://wiki.haskell.org/Maybe) datatype.
    ```haskell
    ghci> :info Maybe
    data Maybe a = Nothing | Just a         -- Defined in ‘GHC.Base’

    ghci> :{
    Prelude| f :: Bool -> Maybe Int
    Prelude| f False = Just 0
    Prelude| f _ = Nothing
    Prelude| :}
  |
    ghci> f False
    Just 0
    ghci> f True
    Nothing
    ```
    - `Maybe` is a sum type consisting of two values, `Just a` which parametrically polymorphic in argument `a` and `Nothing`. It is frequently used to encapsulate results which may not return a defined value of type `a`.
    - Note the type of the above function, `Bool -> Maybe Int`. The following will not parse:
        ```haskell
        ghci> :{
        Prelude| f :: Bool -> Maybe Int
        Prelude| f False = 0
        Prelude| f _ = Nothing
        Prelude| :}

        <interactive>:66:11: error:
            • No instance for (Num (Maybe Int)) arising from the literal ‘0’
            • In the expression: 0
              In an equation for ‘f’: f False = 0
        ```
    - Let's revisit our implementation of **factorial**, which we bottomed with an error for negative numbers (see [factorial-maybe.hs](scratch/factorial-maybe.hs)) and let's try and make a pure function out of it.
        ```haskell
        module FactorialMaybe where

        import Data.Maybe -- we need the function fromJust

        factorial :: (Integral a) => a -> Maybe a
        factorial n
            | n < 0     = Nothing
            | n == 0    = Just 1
            | otherwise = Just $ (*n) . fromJust . factorial $ (n-1)

        -- ...
        ghci> :l scratch/factorial-maybe.hs
        [1 of 1] Compiling FactorialMaybe   ( scratch/factorial-maybe.hs, interpreted )
        ghci> factorial (-1)
        Nothing
        ghci> factorial (8)
        Just 40320
        ghci> factorial (4)
        Just 24
        ghci> factorial (3)
        Just 6
        ghci> factorial (4)
        Just 24
        ghci> factorial (5)
        Just 120
        ghci> factorial (6)
        Just 720
        ```

## Fibonacci Numbers

* We touched briefly on the Fibonacci sequence (extended to negative integers) in the [previous chapter](../chapter07/README.md) to introduce various flow control statements. It is completely defined on the domain of integers, so unlike factorial we need not use a data structure like Maybe to avoid bottom conditions. Let's inspect the [pattern matching example](../chapters07/scratch/fib-pattern.hs).
    ```haskell
    module FibPattern where

    fib' :: Integral a => a -> a
    fib' (-1) = 1
    fib' 0    = 1
    fib' 1    = 1
    fib' n    = (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))
    ```
    - Main take away here is that not only can we apply the function `fib'` recursively, but we can compose it multiple times as well.

## Integral division from scratch

* We hide an accumulator from the calling scope of a function with a bit of indirection, defining nested function within a **where** clause to carry accumulation for us (see [division-integral.hs](scratch/division-integral.hs)):
    ```haskell
    module DivisionIntegral where

    div' :: (Integral a) => a -> a -> (a,a)
    div' n d = div'' n d 0
      where div'' n' d' count
              | n' < d' = (count, n')
              | otherwise = div'' (n' - d') d' (count + 1)

    -- ...
    ghci> :l scratch/division-integral.hs
    [1 of 1] Compiling DivisionIntegral ( scratch/division-integral.hs, interpreted )
    Ok, modules loaded: DivisionIntegral.
    ghci> 9 `div` 5
    1
    ghci> 9 `div'` 5
    (1,4)
    ```
    - The text recommends naming this interior function `go`. I called it `div''`, but I'm crazy like that.

## Additional reading

1. ["Fixed point combinator"](), [Wikipedia](https://en.wikipedia.org)
1. E. Z. Yang, ["The Y Combinator and strict positivity"](http://blog.ezyang.com/2012/09/y-combinator-and-strict-positivity/)
1. ["Factorial"](https://en.wikipedia.org/wiki/Factorial), [Wikipedia](https://en.wikipedia.org)
1. ["Maybe"](https://wiki.haskell.org/Maybe), [Haskell Wiki](https://wiki.haskell.org)
