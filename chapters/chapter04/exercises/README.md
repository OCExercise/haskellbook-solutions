# Exercise Solutions

Review the [notes here](../README.md).

## Mood Swing

Correct the following:

```
changeMood Mood = Woot
changeMood    _ = Blah
```

Solution:

```
changeMood Blah = Woot
changeMood    _ = Blah
```

See [mood.hs](mood.hs):

## Find the mistakes

1. `not True && true`
    * `not True && True`
1. `not (x = 6)`
    * `not (x == 6)`
1. `(1 * 2) > 5`
    * Compiles just fine
1. `[Merry] > [Happy]`
    * These data constructors are not in scope, but even if they were, you'd still need define **instances** of `Ord a` for them. Since we still have to learn about instances, fuggedaboutit.
1. `[1,2,3] ++ "look at me"`
    * No instance defined for concatenating `Num` and `Char`. `(Char Char)` is probably expected, so:
    ```
    ghci> ['1','2','3'] ++ "look at me"
    "123look at me"
    ```

## Chapter Exercises

### General

Define the following before answering:
```
awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]
```

1. What is the type signature of `length`, defined to take a list and return a count of elements in the list?
    * `length :: [a] -> Int`: This is actually not the case in later GHC versions (assuming it ever was). But since we haven't learned about `Foldable` yet, I'm leaving this be.
1. What are the results of the following expressions?
    * `length [1, 2, 3, 4, 5]`: `5`
    * `length [(1, 2), (2, 3), (3, 4)]`: `3`
    * `length allAwesome`: 2
    * `length (concat allAwesome)`: 5
1. Determine which will error and why
    ```
    ghci> 6 / 3
    ...
    ghci> 6 / length [1,2,3]
    ```
    * `6 / length [1,2,3]` will fail as its denominator is of type `Int`, whereas '(/)' has the following signature
    ```
    (/) :: Fractional a => a -> a -> a
    ```
1. How can we fix `6 / length [1,2,3]`?
    * Well, we can upcast the result of `length [1,2,3]` to `Num` using `fromIntegral`, and which will downcast to `Fractional` on the `(/)` operation:
    ```
    ghci> 6 / fromIntegral (length [1,2,3])
    2.0
    ghci> :t fromIntegral
    fromIntegral :: (Num b, Integral a) => a -> b
    ```
1. `2 + 3 == 5` evaluates to a `Bool`, the result being `True`
1. `x + 3 == 5` where `x = 5` evaluates to a `Bool`, with its value being `False`
1. Which of the following will work? Why not? If they work, reduce them:
    * `length allAwesome == 2`: Evaluates to `True`
    * `length [1, 'a', 3, 'b']`: Does not compute. Mixed 'Char' and 'Num' elements
    * `length allAwesome + length awesome`: Evaluates to `5`.
    * `(8 == 8) && ('b' < 'a')`: Evaluates to `False` (`'b' < 'a'` is `False`)
    * `(8 == 8) && 9`: Does not compute. Cannot logically AND a `Bool` and a `Num`
1. Implement a function to test if a string or list is a palindrome.
    * Strings are lists, and we have this nice function `reverse [a] -> [a]`, so:
    ```
    isPalinDrome :: (Eq a) => [a] -> Bool
    isPalindrome x = x == reverse x
    ```
1. Implement the absolute function
    * Trivial:
    ```
    myAbs :: Integer -> Integer
    myAbs n = abs n
    ```
    * Actually implementing it:
    ```
    myAbs :: Integer -> Integer
    myAbs n = if n < 0 then n * (-1) else n
    ```
1. Fill in the definition of the following function, using tuple functions `fst` and `snd`
    ```
    f::(a,b)->(c,d)->((b,d),(a,c))
    f = undefined
    ```
    * Not sure how to do this point free, so:
    ```
    f :: (a,b) -> (c,d) -> ((b,d), (a,c))
    f x y = ((snd x, snd y), (fst x, fst y))
    ```

### Correcting syntax

1. The following capitalizes function name `F` improperly and uses single-quotes instead of backticks to infix operator `x`.
    ```
    x = (+)
    F xs = w 'x' 1 where w = length xs

    -- becomes

    x = (+)
    f xs = w `x` 1 where w = length xs
    ```
1. Fix this lambda expression: `\ X = x`
    * `(\x -> x)`
1. Fix this lambda expression: `\ x : xs -> x`
    * `(\xs -> head xs)`
1. Fix this tuple selector: `f (a b) = A`
    * `f t = fst t`
    * `f (a,b) = a`
1.

### Match the function names to their types

1. Type signature of `show`
    * **c)**: `Show a => a -> String`
1. Type signature of `(==)`
    * **b)**: `Eq a => a -> a -> Bool`
1. Type signature of `fst`
    * **a)**: `(a,b) -> a`
1. Type signature of `(+)`
    * **d)**: `Num a => a -> a -> a`
