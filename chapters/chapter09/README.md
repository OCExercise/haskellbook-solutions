# Lists

## Directions

* [Table of Contents](../../README.md)
* Solutions availables [here](exercises/README.md).

## Introduction

Finally, lists! We're going to cover a range of topics, including:
* the list data type,
* some essential functions that operate on lists (aside from [fold](../chapter10/README.md), at least not in detail),
* list comprehensions,
* pattern matching on lists,
* deconstructing and assembling lists (for folks who really miss [car, cdr](https://en.wikipedia.org/wiki/CAR_and_CDR), and [cons](https://en.wikipedia.org/wiki/Cons) from the good old days of [Scheme](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html#%_sec_2.1.1))
* and some stuff I've never heard of before.

## The list datatype

* Yes, lists execute a [data type](../chapter04/README.md).
    ```haskell
    ghci> :info []
        data []   a = []   |   a : [a]        -- Defined in ‘GHC.Types’
    --  |______| |_|  |_| |_| |_______|
    --      |     |    |   |      |
    --      1     2    3   4      5
    ```
    1. Data type constructor
    2. Data type constructor argument
    3. Representation of the empty list (the [**nullary** constructor](https://en.wikipedia.org/wiki/Nullary_constructor))
    4. Indicates that this is a [**sum type**]() as we've seen before in [Chapter 4](../chapter04/README.md#data-declarations)
    5. A value of polymorphic type `a` [cons'd](https://en.wikipedia.org/wiki/Cons) to a list of values of the same type `a`.
        - [cons `(:)`](https://en.wikipedia.org/wiki/Cons) is a infix function, like the tuple operator `(,)`, applies to two arguments:
            ```haskell
            ghci> :t (:)
            (:) :: a -> [a] -> [a]
            ghci> :t (,)
            (,) :: a -> b -> (a, b)
            ghci> (:) 1 [2]
            [1,2]
            ghci> (,) 1 2
            (1,2)
            ghci> 1:[2]
            [1,2]
            ghci> (1,2)
            (1,2)
            ```
        - The definition of the n-arity list `[a]` is recursively defined in the data type declaration for list, unlike tuple.

## Pattern matching on Lists

* As we learned in [Chapter 7](../chapter07/README.md#pattern-matching), pattern matching operates on the contructors of data types; lists are no exception (see [tail.hs](scratch/tail.hs)).
    ```haskell
    head' :: [a] -> a
    head' []    = error "empty list"
    head' (x:_) = x

    tail' :: [a] -> [a]
    tail' []        = error "empty list"
    tail' (_:[])    = error "empty list"
    tail' (_:xs)    = xs

    -- ...

    ```
    - An exhaustive pattern match on a list typically considers the empty list case `[]` and an exhaustive match on the head or tail of a `(:)` operation. In this case, we throw an error much like the default implementation of the functions [`head`](https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:head) and [`tail`](https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:tail).
    - We can use [`Maybe`](https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#t:Maybe) to avoid the [bottom](../chapters08/README.md#bottom) behavior in our straightforward implementation:
    ```haskell
    head'' :: [a] -> Maybe a
    head'' []     = Nothing
    head'' (x:_)  = Just x

    tail'' :: [a] -> Maybe [a]
    tail'' []     = Nothing
    tail'' (_:[]) = Nothing
    tail'' (_:xs) = Just xs
    ```


## List's syntactic sugar

* We've encountered some built-in syntactic sugar pertaining to lists in previous chapters, but let's list a few key ones here:
    - Appending elements to a list:
        ```haskell
        ghci> 1:[2..10]
        [1,2,3,4,5,6,7,8,9,10]
        ```
    - String concatenation:
        ```haskell
        -- String is [Char]
        ghci> 'A':" quick brown fox did something and screw it."
        "A quick brown fox did something and screw it."
        ```
    - Concatenating two lists
        ```haskell
        ghci> [1..3] ++ [4]
        [1,2,3,4]

        -- compare with
        ghci> (1 : 2 : 3 : []) ++ 4:[]
        [1,2,3,4]
        ```
    - Selecting an element of a list by its index
        ```haskell
        ghci> "cat" !! 0
        'c'
        ghci> "cat" !! 4
        *** Exception: Prelude.!!: index too large
        ```

## Using ranges to construct lists

* **Ranges** allow us to construct lists through use of the `..` facility (is this not an operator?). Provided the elements of a list dispatch [`Enum a => a`](../chapter06/README.md#Enum), we can construct ranges as follows:
    - Integer increment within a closed interval:
        ```haskell
        ghci> [1..10]
        [1,2,3,4,5,6,7,8,9,10]
        ```
    - Hinted integer increment (unnecessary):
        ```haskell
        ghci> [1,2..10]
        [1,2,3,4,5,6,7,8,9,10]

        ghci> [1,2..10] == [1..10]
        True
        ```
    - Hinted odd numbers on a half open interval
        ```haskell
        ghci> [1,3..10]
        [1,3,5,7,9]
        ```
    - Hinted even numbers on a closed interval
        ```haskell
        ghci> [2,4..10]
        [2,4,6,8,10]
        ```
    - Integer decrement a closed interval traversing `0`
        ```haskell
        ghci> [10,9..(-10)]
        [10,9,8,7,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]
        ```
   - Character increment and decrement (the latter requires a hint):
        ```haskell
        ghci> ['a'..'e']
        "abcde"
        ghci> ['e'..'a']
        ""
        ghci> ['e','d'..'a']
        "edcba"
       ```
## Extracting portions of lists

## List comprehensions

## Spines and nonstrict evaluation

## Transforming lists of values

## Filtering lists of values

## Zipping lists

## Additional reading


1. ["Introduction to Data Abstraction"](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html#%_sec_2.1.1), [*Structures and Interpretation of Computer Programs*](https://mitpress.mit.edu/sicp/full-text/book/book.html)
1. ["Nullary constructor"](https://en.wikipedia.org/wiki/Nullary_constructor), [Wikipedia](https://en.wikipedia.org)
