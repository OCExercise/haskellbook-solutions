# Types

Let's learn more about Haskell's built in datatypes and also how to create our own. We'll go beyond the simple constructions of the last chapter, exploring:

* type signatures
* currying
* polymorphism
* type inference
* type declaration

We're going to delve deeper into several symbols in this chapter. Googling for symbols sucks. [Hoogling](https://www.haskell.org/hoogle/), on the other hand, is nice. Try Hoogling [`(->)`](https://www.haskell.org/hoogle/?hoogle=%28-%3E%29) for example.

## Chapter Exercises

Solutions availables [here](exercises/README.md).


## Recap: Type and Data constructors

```haskell
    data Foo = Bar | Buzz
--  |______|  |__________|
--      |          |
--      1          2
```
1. **Type constructor**: Declares a datatype and binds to a name, in this case `Foo`.
2. **Data constructors**: `Bar` and `Buzz` are data constructors. The pipe symbol (`|`) represents a disjunction of the two. That is, the datatype `Foo` can take on either value `Bar` **or** `Buzz`.

We will explore additional type and data constructor declarations, including [parametrized ones](https://wiki.haskell.org/Constructor).

## What are types for?

* Haskell implements a [pure lambda calculus](../chapter01/README.md). Specifically, a [typed one](https://en.wikipedia.org/wiki/Typed_lambda_calculus).
* A **type** is a property of a **term** which constrains its value and operations that can be performed on it. This is my definition, largely derived from [the wiki](https://wiki.haskell.org/Type)'s, so take it with a grain of salt. I've yet to come across a widely accepted mathematical formulation, though there are hints of some from various fields in math and computer science.
* In Haskell, types allow programmers to apply explicit constraints to ensure correctness.
* As a **statically typed language**, Haskell can capture type constraint errors at compile time.
* Lots of other goodies come with typing, but we'll skip these for now and get to learning how to use and construct types.

## How to read type signatures

* A [type signature](https://wiki.haskell.org/Type_signature) explicitly specifies the type of a variable. It is expressed in the form:
    ```haskell
    <name/value> :: <type>
    ```
    Some examples:
    ```haskell
    'c' :: Char
    "hello" : [Char]
    9 :: Num t => t
    [9] ::

    ...
    -- Constrains x to typeclass Num
    x :: Num t => [t]
    x = 9

    -- Constrains y to data type Bool
    y :: Bool
    y = True

    -- Constrains f to operate on a single argument of typeclass Num
    f :: Num a => a -> a
    f x = x * 3

    -- Constrains g to operate on two arguments of arbitrary types a and b
    g :: a -> b -> (a, b)
    g a b = (a, b)
    ```
* The last value in a type signature is the ultimate result.
    ```haskell
    -- The result is the last 'a', which constrained constrained to typeclass Num
    f :: Num a => a -> a
    f x = x * 3

    -- The result is the tuple (a,b)
    g :: a -> b -> (a, b)
    g a b = (a, b)
    ```
### Understanding the function type

* [`(->)`](https://wiki.haskell.org/Keywords#-.3E) is a **function type constructor**:
    ```haskell
    ghci> :info (->)
    data (->) t1 t2         -- Defined in ‘GHC.Prim’
    infixr 0 `(->)`
    instance Monad ((->) r) -- Defined in ‘GHC.Base’
    instance Functor ((->) r) -- Defined in ‘GHC.Base’
    instance Applicative ((->) a) -- Defined in ‘GHC.Base’
    instance Monoid b => Monoid (a -> b) -- Defined in ‘GHC.Base’
    ```
    Given two types, `t1` and `t2`, `(->)` constrains implementations to map `t1` to `t2`.
    ```haskell
    -- head maps list of arbitrary type a (or simply [a]) to a
    ghci> :t head
    head :: [a] -> a
    ```
* `(->)` is an **infix** operator that associates to the right. **NOTE**: applying the function associates to the left.
* Let's break down a type signature more comprehensively than we have before. The text uses `fst` and `length` as examples, but let's use something more complicated--say `product`--instead.
    The `product` function that operates on types that are [instances]() of the [type class](#Type-class-constrained-type-variables) [Foldable]() (we'll get into [instances]() and [type classes]() later) with constituents of type `Num a`. A [list]() is of type [Foldable](), and `product` can operate on any list with constituents (elements) that are numbers.
    ```haskell
    ghci> product 2 [1,2,3]
    6
    ```
    Let's look at the type signature and break it down.
    ```haskell
        product  ::  (Num a, Foldable t)  =>  t  a   -> a
    --  |_____| |__| |_________________| |__| |___| |_____|
    --     |     |            |           |     |      |
    --     1     2            3           4     5      6
    ```
    1. Name of the function. In this case, `product`.
    2. `::` is nn annotation operator, represented infix, which relates a name `foo` (to the left) with its type and type constraints (i.e., `a`, `Int`, `Ord a`). This one is difficult to Hoogle. In fact, I have not been able to find any authoritative documentation on it at all.
    3. The [instance context]() specifies of type constraints on variables used within the function.
    4. [`(=>)`](https://wiki.haskell.org/Keywords#.3D.3E), represented infix, relates an [instance context]() to subsequent flow of function application to arguments.
    5. `t a` can be considered a generalization of syntactic sugar we've seen previously; namely, `[a]`. We'll learn more about this sort of syntax later.
    6. The result.

### Type class-constrained type variables

* From [Wikipedia](https://en.wikipedia.org), [type classes](https://en.wikipedia.org/wiki/Type_class) are constructs used to represent [ad-hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism).  We often want to define functions that operate on several different but related concrete representations. If for example, imagine if we defined a multiplication operator like so:
    ```haskell
    (*) :: Int -> Int -> Int
    ```
    Unfortunately, this would not be useful for numbers that are type `Float`, or `Fractional`, or any other numeric type we'd like to multiply. Fortunately, we can leverage **type class** support for **polymorpism** to abstract beyond the integers. In this case, we make use of the **type class* `Num`:
    ```haskell
    (*) :: Num a -> a -> a -> a
    ```
    `a` is called a **type variable** in this case. As the name suggests, it can take on any data type that [instances]() the **type class** `Num` (more in [instancing]() later).
* We previously noted that the annotation `(::)` is used to signal the type of some named thing. That thing was a **function** in previous examples, but variables literals can also be annotated by type.  For example:
    ```haskell
    ghci> let x_int = 15 :: Int
    ghci> :t x_int
    x_int :: Int

    ...

    ghci> let y_double = 15 :: Int
    ghci> :t y_double
    x_double :: Int

    ...

    ghci> let x_int = 15 :: Int
    ghci> x_int * x_int
    225
    ghci> y_double * y_double
    225.0

    ```
   So far so good, but what happens if you attempt to mix `x_int` and `y_double` in an arithematic expression?
    ```haskell
    ghci> x_int * y_double

    <interactive>:54:9: error:
        • Couldn't match expected type ‘Int’ with actual type ‘Double’
        • In the second argument of ‘(*)’, namely ‘y_double’
          In the expression: x_int * y_double
          In an equation for ‘it’: it = x_int * y_double
    ```

### Currying

* All functions in Haskell, as in lambda calculus, operate (apply to) one and only one argument. Multiple arguments, as in lambda calculus, may be represented by some syntactic sugar, but at the end of the day any such multiple argument function is simply the successive application of functions to the left against the first of the remaining arguments to the right.

#### My own diversion into folding

* The text introduces currying by examining the type `(+)`. Let's take a look instead at the type for `foldl`, a very useful function that applies a **partially applied function** to some `Foldable t` from the left ([left-associative]()). Before we dive into currying, let's discuss the function a bit:
    ```haskell
    ghci> foldl (*) 1 [1,2,3]
    6
    ghci> sum'

    ```
    And it's type signature:
    ```haskell
            foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
    ```
* There is a corresponding `foldr` that is [right-associative]():
    ```haskell
    Let's take a look at it's type signature:
    ```haskell
            foldl :: Foldable t => (a -> b -> b) -> b -> t a -> b
    ```
    You may note that the first argument to `foldr`--`(a -> b -> b)`--differs from the `(b -> a -> b)` we saw for `foldl`. You can essentially consider the value represented by `b` as an **accumulator** and `a` the next element in the list.  The order matters, and as is readily apparent when we examine `foldl` and `foldr` with non-commutative operations like subtraction, division and integer division:
    ```haskell
    ghci> foldl (\y x -> y - x) 0 [1]
    -1
    ghci> foldr (\y x -> y - x) 0 [1]
    1
    ghci> foldl (\y x -> y / x) 0 [1]
    0.0
    ghci> foldr (\y x -> y / x) 0 [1]
    Infinity
    ghci> foldl (\y x -> (div) y x) 0 [1]
    0
    ghci> foldr (\y x -> (div) y x) 0 [1]
    *** Exception: divide by zero

    ```
    The expressions `(\y x -> ...)` are examples of [anonymous functions](). We could easily replace these with operations that adhere to the portion of the signature given by `(b -> a -> b)`.

#### Back on track: currying

* Again let's look at `foldl`.  As practically applied, it appears to be a three argument function:
    ```haskell
        foldl (\y x -> y - x)  0  [1,2,3,4,5]
    --- |___| |_____________| |_| |_________|
    ---   |          |         |        |
    ---   1          2         3        4
    ```
    Where we have:
    1. the function call,
    2. the first argument, which is a function,
    3. the second argument, which is a number (the **accumulator**, in fact), and finally
    4. the third argument, which is the list which we want to fold.
* Let's re-examine its type signature:
    ```haskell
            foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
    ---     |___|   |________|     |___________|  |___| |____| |_|
    ---       |          |               |          |     |     |
    ---       1          2               3          4     5     6
    ```
    Broken down, we have:
    1. the function name,
    2. the **instance context**, which constrains the type variable `t` to some instance of **type class** `Foldable`,
    3. the first argument, which represents some function that takes variables of type `a` and `b`,
    4. the second argument, which is constrained to type `b` referenced in the first argument,
    5. the third argument, which constrains the constituents of `Foldable` type `t` to type `a`, and finally
    6. the result, which is of type `b`.

#### Partial application

#### Manual currying and uncurrying

#### Sectioning

### Polymorphism

#### Polymorphic constants

#### Working around constraints

### Type inference

### Asserting types for declarations


## Additional reading

##### Typed Lambda Calculus

1. ["Typed lambda calculus"](https://en.wikipedia.org/wiki/Typed_lambda_calculus), [Wikipedia](https://en.wikipedia.org)
1. ["Simply typed lambda calculus"](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus), [Wikipedia](https://en.wikipedia.org)
1. Murawski, A. S., ["Typed Lambda Calculi"](http://www.cs.le.ac.uk/people/amurawski/mgs2011-tlc.pdf)
1. ["Simply-Typed -Calculus"](http://www.cs.cornell.edu/courses/cs6110/2013sp/lectures/lec25-sp13.pdf), CS 6110 Lecture 25, Cornell University

##### Haskell

1. ["Types"](https://wiki.haskell.org/Type), ["Haskell Wiki"](https://wiki.haskell.org/Type)
1. ["Type signatures"](https://wiki.haskell.org/Type_signature), ["Haskell Wiki"](https://wiki.haskell.org)
1. ["Constructor"](https://wiki.haskell.org/Constructor), ["Haskell Wiki"](https://wiki.haskell.org)
1. Lipovača, Miran, ["Types and Type Classes"](https://http://learnyouahaskell.com/types-and-typeclasses), ["Learn You A Haskell for Great Good!"](https://http://learnyouahaskell.com/)
1. Diehl, Stephen, ["Type Systems"](http://dev.stephendiehl.com/fun/type_systems.html), ["Write You A Haskell"](http://dev.stephendiehl.com/fun/index.html)

## Assorted problems
