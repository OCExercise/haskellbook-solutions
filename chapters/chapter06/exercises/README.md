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

## Chapter Exercises

### Multiple Choice

### Does it typecheck?

### Given a datatype declaration, what can we do?

### Match the types

### Type-Kwon-Do Two: Electric Typealoo

We all saw that coming.
