# Exercise Solutions

## Directions

* [Table of Contents](../../../README.md)
* Review the [notes here](../README.md).

## Eq Instances

Infer the `Eq` instance declaration for the supplied data types:

1. `data TisAnInteger`
    ```haskell
    data TisAnInteger = TisAn Integer

    instance Eq a => Eq (TisAn a) where
        (==) (TisAn Integer) (TisAn Integer) =
    ```
## Will they work?

Determine if the following `Ord` operations will work on the operands supplied.

1. `max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])`
    * Yes.
    ```haskell
    ghci> max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
    5
    ```
1. `compare (3 * 4) (3 * 5)`
    * Yes.
    ```haskell
    ghci> max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
    5
    ```
1. `compare "Julie" True`
    * No. Compare expects two arguments of the same type. `"Julie"` is `[Char]`, and `True` is `Bool`.
    ```haskell
    ghci> compare "Julie" True

    <interactive>:68:17: error:
        • Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
        • In the second argument of ‘compare’, namely ‘True’
          In the expression: compare "Julie" True
          In an equation for ‘it’: it = compare "Julie" True

    ```
1. `compare (3 * 4) (3 * 5)`
    * Yes.
    ```haskell
    ghci> (5 + 3) > (3 + 6)
    False
    ```

## Chapter Exercises

### Multiple Choice

### Does it typecheck?

### Given a datatype declaration, what can we do?

### Match the types

### Type-Kwon-Do Two: Electric Typealoo

We all saw that coming.
