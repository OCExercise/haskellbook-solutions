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
        - We'll see how we can recover from this later when we dive into [factorial](#factorial) and [Fibonacci sequences](#fibonacci-numbers).
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
    * errors on anything other than non-negative input,
    * terminates quickly once its argument matches 0, and
    * recurses for any other input.
* Remove the error state of the above function would result in non-terminating recursion.
    ```haskell
    ghci> :{
    Prelude| factorial n
    Prelude|     | n == 0 = 1
    Prelude|     | otherwise = n * factorial(n-1)
    Prelude| :}
    ghci> factorial -1

    <interactive>:50:1: error:
        • Non type-variable argument in the constraint: Num (t -> t)
          (Use FlexibleContexts to permit this)
        • When checking the inferred type
            it :: forall t. (Eq t, Num (t -> t), Num t) => t -> t
    ghci> factorial (-1)
    ^CInterrupted. -- just hangs
    ```




## Bottom

## Fibonacci Numbers

## Integral division from scratch

## Additional reading

1. ["Fixed point combinator"](), [Wikipedia]()
1. E. Z. Yang, ["The Y Combinator and strict positivity"](http://blog.ezyang.com/2012/09/y-combinator-and-strict-positivity/)
