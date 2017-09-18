# Exercise Solutions

## Directions

* [Table of Contents](../../../README.md)
* Review the [notes here](../README.md).

## Scope

1. Determine if `y` is in scope (globally or locally) for `z`? **Yes**. `y` is in the global scope of `z`
    ```haskell
    let x = 5
    let y = 7
    let z = x * y
    ```
1. Is `h` in scope for `g`? **No**: `h` is not defined prior to `g`, or at all in this case.
    ```haskell
    let f = 3
    let g = 6 * f + h
    ```
1. Is `r` in scope for `area`? **No**. `r` is not locally bound to `area`, and its global definition postdates `area`'s.
    ```haskell
    area d = pi * (r * r)
    r = d/2
    ```
1. Are `d` and `r` in scope for `area`? **Yes**. `d` is locally bound as an argument and `r` is locally bound to a where clause.
    ```haskell
    area d      = pi * (r * r)
        where r = d / 2
    ```

## Syntax Errors

Correct the syntax.

1. `++ [1, 2, 3] [4, 5, 6]` should be `(++) [1, 2, 3] [4, 5, 6]`.
1. `'<3' ++ ' Haskell'` should be `"<3" ++ " Haskell"`.
1. `concat ["<3", " Haskell"]` is already correct.

## Chapter Exercises

### Reading Syntax

1. Fix or evaluate the following (p. 112-3):
    1. `concact [[1,2,3,],[4,5,6]]` => `[1,2,3,4,5,6]`
    1. `++ [1,2,3] [4,5,6]`
        * Above errors. Should read: `(++) [1,2,3] [4,5,6] => [1,2,3,4,5,6]`
    1. `(++) "hello" " world"` => `"hello world"`
    1. `["hello" ++ " world]`
        * Missing closing double-quote. Should read: `["hello" ++ " world"]`
    1. `4 !! "hello"`
        * Incorrect use of the operator `(!!)`. Should read: `"hello" !! 4`
    1. `(!!) "hello" 4 => `'o'`
    1. `take "4 lovely"`
        * `take :: Int -> [a] -> [a]`. Should read `take 4 "lovely"`
    1.`take 3 "awesome"` => `"awe"`
1. Match the following (p. 113)
    1. `concat [[1 * 6], [2 * 6], [3 * 6]]` => [6,12,18]
    1. `"rain" ++ drop 2 "elbow"` => `"rainbow"`
    1. `10 * head [1, 2, 3]` => `10`
    1. `(take 3 "Julie") ++ (tail "yes")` => `"Jules"`
    1. `concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]` => `[2,3,5,6,8,9]`

### Building Functions

1. Implement functions to perform the following
    1. `"Curry is awesome" => "Curry is awesome!"`
        ```haskell
        appendExclamation s = s ++ "!"
        ```
    1. `"Curry is awesome" => "y"`
        ```haskell
        -- trivial (deliberately missing the point, but permitted
        -- by the question as phrased)

        printY s = "y"

        -- Return the nth character (as a single element list).
        -- This solution is probably the intended take away
        -- from this chapter. Set n = 4 to get the answer

        elemAtIndex n s = s !! n

        -- Find character 'c' in a string ([Char]). Return
        -- "c" (note [Char]) if discovered, otherwise
        -- return "". This is not a very useful function.
        -- Useful reworks include:
        -- 1. Testing whether a character is present in a string
        -- 2. Returning the index of a character in the string, or
        --    -1 if not present.

        import Data.List
        findCharacter c s
            | m == (Just c) = [c]
            | otherwise = []
            where m = find (==c) s
        ```
    1. `"Curry is awesome" => "y"`
        ```haskell
        -- trivial (deliberately missing the point, but permitted
        -- by the question as phrased)
        printAwesome s = "awesome"

        -- Takes and drops. Probably the intended take away
        -- from this chapter. Set n = 7, m = 9 and
        -- s = "Curry is awesome!" to get the answer
        takeDrop n m s = take n $ drop m s

        -- We'll use "isInfixOf" from Data.List.
        -- Also not a terribly useful function.

        import Data.List
        findSubstring sub s
            | sub `isInfixOf` s = sub
            | otherwise = ""

        ```
