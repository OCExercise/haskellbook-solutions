# Type Classes

## Directions

* [Table of Contents](../../README.md)
* Solutions availables [here](exercises/README.md).

## Overview

* This chapter will define type classes and give special attention given to certain examples:
    1. `Bool`,
    2. `Eq`,
    3. `Num`,
    4. `Prd`,
    5. `Enum`,
    6. `Show`, and
    7. `Read`
* We will also learn about:
    * type-defaulting type classes,
    * type class inheritance

## What are type classes ?

* **Problem**: We have a set of [data types](../chapter04/README.md) that should be operated upon similarly. How do we do that?
    * **Solution**: We use **type classes**.
* From [Wikipedia](https://en.wikipedia.org), [Type classes](https://en.wikipedia.org/wiki/Type_class) are the mechanism by which we constrain [parametrically polymorphic types](../chapter05/README.md#Polymorphism).
* [Parametrically polymorphic types](../chapter05/README.md#Polymorphism) are useful, but only a limited number of operations can be applied to them. Type class constraints permit a richer field of operations to be applied, for example:
    * Addition `(+)` and multiplication `(*)` are defined for members of the type class `Num a => a`
    * Comparison operators `(>)`, `(<)` are defined for members of the type class `Ord a => a`
* **Type classes** declare operations.
* **Instances** supply implementation for operations declared in **typeclasses**
* **NOTE**: This is a bit of a spoiler alert, since we haven't gotten to **instances** yet, but type classes are **not** a magic way to define an operation **exactly once** such that it works on all conceivable data types assigned to a type class. **Inheritance** saves us a fair bit of time and work, but there will be cases where you must define new behavior for an operation (similar to [function overloading](https://en.wikipedia.org/wiki/Function_overloading)).

## Back to Bool

* [Data types](../chapter04/README.md) can be constrained by type classes given the following specification (`:info` leaves out the implementation, so don't forget it):
    ```haskell
    ghci> :info Bool
    data Bool = False | True        -- Defined in ‘GHC.Types’
    instance Bounded Bool -- Defined in ‘GHC.Enum’
    instance Enum Bool -- Defined in ‘GHC.Enum’
    instance Eq Bool -- Defined in ‘GHC.Classes’
    instance Ord Bool -- Defined in ‘GHC.Classes’
    instance Read Bool -- Defined in ‘GHC.Read’
    instance Show Bool -- Defined in ‘GHC.Show’
    ```
* **Instances** implement a definition of *how* a data type adheres to its type class, canonically by providing an implementation of an operation defined in a type class. We will get to how those operations are declared in a type class in the first place in [a little bit](#Eq).
* Note that the data type is instrumented with several invocations of the keyword `instance` followed by a type class constraint and the data type's name. We will learn how to define these ourselves [a bit later](#Writing-type-class-instances).
* Refer to the text for details on what these type class constraints on `Bool` actually mean. Refer to [Hackage](http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#t:Bool) for `Bool`'s precise specification and source. Main takeaway is that actual work had to be done to define these instances. For example:
    ```haskell
    ------------------------------------------------------------------------
    -- Bool
    ------------------------------------------------------------------------

    -- | @since 2.01
    deriving instance Bounded Bool

    -- | @since 2.01
    instance Enum Bool where
      succ False = True
      succ True  = errorWithoutStackTrace "Prelude.Enum.Bool.succ: bad argument"

      pred True  = False
      pred False  = errorWithoutStackTrace "Prelude.Enum.Bool.pred: bad argument"

      toEnum n | n == 0    = False
               | n == 1    = True
               | otherwise = errorWithoutStackTrace "Prelude.Enum.Bool.toEnum: bad argument"

      fromEnum False = 0
      fromEnum True  = 1

      -- Use defaults for the rest
      enumFrom     = boundedEnumFrom
      enumFromThen = boundedEnumFromThen
    ```
* Type classes can encompass other type classes, for example:
    * `Num a => a` encompasses all members of
        * `Integral a => a`,
        * `Fractional a => a`,
        * `Real a => a`,
    * `Eq a => a` encompasses all members of `Ord a => a`

## Eq

* As mentioned previously,
    1. type classes declare operations, and
    2. instances supply the needed implementation.
* Let's take a look at `Eq` to reinforce the point:
    ```haskell
    ghci> :info Eq
    class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool

    -- a bunch of instance declarations follow, we'll ignore them for now
    ```
    * `Eq` declares two operations:
        1. `(==)` equivalence
        1. `(/=)` non-equivalence
    * Instances would bind these operations to data types and provide their implementation.
* Work through the examples in this section of the text. We won't dwell on them.
* We can [derive](https://wiki.haskell.org/Keywords#deriving) automagically from a few type classes that are built into Haskell, including `Eq`, `Ord`, `Enum`, `Bounded`, `Read` and `Show`.

## Writing type class instances



## Num

## Type-defaulting type classes

## Ord

## Enum

## Show

## Read

## Instances are dispatched by type

## Gimme more operations

## Additional Reading

1. ["Type class"](https://en.wikipedia.org/wiki/Type_class), [Wikipedia](https://en.wikipedia.org)
1. ["OOP vs type classes"](https://wiki.haskell.org/OOP_vs_type_classes), [Haskell Wiki](https://wiki.haskell.org)
1. ["Research Papers: Type classes"](https://wiki.haskell.org/Research_papers/Type_systems#Type_classes), [Haskell Wiki](https://wiki.haskell.org)
1. Lipovača, Miran, ["Types and Type Classes"](https://http://learnyouahaskell.com/types-and-type classes), [Learn You A Haskell for Great Good!](https://http://learnyouahaskell.com/)
