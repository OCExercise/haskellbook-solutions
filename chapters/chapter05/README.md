# Types

## Directions

* [Table of Contents](../../README.md)
* Solutions availables [here](exercises/README.md).

## Overview

Let's learn more about Haskell's built in datatypes and also how to create our own. We'll go beyond the simple constructions of the last chapter, exploring:

* type signatures
* currying
* polymorphism
* type inference
* type declaration

We're going to delve deeper into several symbols in this chapter. Googling for symbols sucks. [Hoogling](https://www.haskell.org/hoogle/), on the other hand, is nice. Try Hoogling [`(->)`](https://www.haskell.org/hoogle/?hoogle=%28-%3E%29) for example.

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
    The `product` function that operates on types that are [instances](https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Classes_and_instances) of the [type class](#Type-class-constrained-type-variables) [Foldable](https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Classes_and_instances) (we'll get into **instances** and **type classes** later) with constituents of type `Num a`. A list is of type [Foldable](https://wiki.haskell.org/Foldable), and `product` can operate on any list with constituents (elements) that are numbers.
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
    2. `::` is an annotation operator, represented infix, which relates a name `foo` (to the left) with its type and type constraints (i.e., `a`, `Int`, `Ord a`). This one is difficult to Hoogle. In fact, I have not been able to find any authoritative documentation on it at all.
    3. The [instance context](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#class-and-instances-declarations) specifies of type constraints on variables used within the function.
    4. [`(=>)`](https://wiki.haskell.org/Keywords#.3D.3E), represented infix, relates an **instance context** to subsequent flow of function application to arguments.
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
    `a` is called a **type variable** in this case. As the name suggests, it can take on any data type that **instances** the **type class** `Num`.
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

* **Currying** is a procedure to translate functions with multiple arguments into a sequence of single argument function applications.
* All functions in Haskell, as in lambda calculus, operate (apply to) one and only one argument. Multiple arguments, as in lambda calculus, may be represented by some syntactic sugar, but at the end of the day any such multiple argument function is simply the successive application of functions to the left against the first of the remaining arguments to the right.

#### My own diversion into folding

* The text introduces currying by examining the type `(+)`. Let's take a look instead at the type for `foldl`, a very useful function that applies a **partially applied function** to some `Foldable t` from the left ([left-associative](https://www.haskell.org/tutorial/functions.html)). Before we dive into currying, let's discuss the function a bit:
    ```haskell
    ghci> foldl (*) 1 [1,2,3]
    6
    ghci> sum'

    ```
    And it's type signature:
    ```haskell
    foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
    ```
* There is a corresponding `foldr` that is [right-associative](https://www.haskell.org/tutorial/functions.html):
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
    The expressions `(\y x -> ...)` are examples of [anonymous functions](https://wiki.haskell.org/Anonymous_function). We could easily replace these with operations that adhere to the portion of the signature given by `(b -> a -> b)`.

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
    --- |___|   |________|     |___________|  |___| |____| |_|
    ---   |          |               |          |     |     |
    ---   1          2               3          4     5     6
    ```
    Broken down, we have:
    1. the function name,
    2. the **instance context**, which constrains the type variable `t` to some instance of **type class** `Foldable`,
    3. the first argument, which represents some function that takes variables of type `a` and `b`,
    4. the second argument, which is constrained to type `b` referenced in the first argument,
    5. the third argument, which constrains the constituents of `Foldable` type `t` to type `a`, and finally
    6. the result, which is of type `b`.
* Recall `(->)` represents the application of a function. The is a **right-associative** infix operator ([`infixr`](https://wiki.haskell.org/Keywords#infixr)). Function application, on the other hand, is **left-associative**. We apply a function (and its subsequent **results** [more on this in the next section]) to the left to arguments to the right until we reduce to normal form.
* The grouping with explicit right-associative parenthesization:
    ```haskell
    foldl :: Foldable t => (b -> (a -> b)) -> (b -> (t a -> b))
    ---                             |____________|    |_|   |________|
    ---                                    |           |         |
    ---                                    1           2         3
    ```
    Let's unpack this.
    1. The first argument, which is some function supplied to `foldl`, first takes an argument `b`, yielding a function that takes an argument `b`, yielding a a result `b`. This is a curried function in and of itself.
    2. The second argument, which is the **accumulator**. The function supplied in 1) is applied to it, yielding yet another function.
    3. The third argument is some variable of type class `Foldable t` which yields the penultimate result of type `b` when the function yielded in 3) is applied to it.
* I repeat. The order of evaluation remains **left-associative**. Burn it into your brain. Don't let the curry fool you.

#### Partial application

* **Partial application** of a function refers to the intermediate results obtained via evaluation prior to reaching normal form for an entire expression of interest. Let's deviate from the text again in our examples.
    ```haskell
    ghci>
    ```
* Partial application is left-associative--that is, a function is applied only to the leftmost term. Consequently, partial application cannot "skip" arguments to yield new functions.
* The result of a partially applied function is a new function:
* Let's try a few examples:
    * Given a function `f(x,y) = x + y`, yield a function `g(5)` that evaluates to `15`:
        ```haskell
        ghci> f x y = x + y
        ghci> :t f
        f :: Num a => a -> a -> a
        ghci> g = f 10
        ghci> :t g
        g :: Num a => a -> a
        ghci> 15 == g 5
        True
        ```
    * Given a function `f(x,y) = x ++ y`, yield a function `g("Prez")` that evaluates to `"hello, Prez"`:
        ```haskell
        ghci> g = f "hello, "
        ghci> :t g
        g :: [Char] -> [Char]
        ghci> "hello, Prez" == g "Prez"
        True
        ```
    * Given a function `f(x,y,z) = foldr x y z`, yield a function `g([1..3])` that multiplies all the elements of the list `[1..3]`:
        ```haskell
        ghci> f x y = x ++ y
        ghci> :t f
        f :: [a] -> [a] -> [a]
        ghci> f x y z = foldr x y z
        ghci> :t f
        f :: Foldable t => (a -> b -> b) -> b -> t a -> b
        ghci> g = f (*) 1
        ghci> :t g
        g :: (Foldable t, Num b) => t b -> b
        ghci> 6 == g [1..3]
        True
        ```
    * Given a function `f(x,y,z) = foldr x y z`, yield a function `g([1..3])` that multiplies all the elements of the list `[1..3]`:
        ```haskell
        ghci> f x y = x ++ y
        ghci> :t f
        f :: [a] -> [a] -> [a]
        ghci> f x y z = foldr x y z
        ghci> :t f
        f :: Foldable t => (a -> b -> b) -> b -> t a -> b
        ghci> g = f (*) 1
        ghci> :t g
        g :: (Foldable t, Num b) => t b -> b
        ghci> 6 == g [1..3]
        True
        ```
    * Given a function `f(x,y,z) = foldr x y z`, yield a function `g([1..3])` that sums all the elements of the list `[1..3]`:
        ```haskell
        ghci> f x y z = foldr x y z
        ghci> :t f
        f :: Foldable t => (a -> b -> b) -> b -> t a -> b
        ghci> g = f (+) 0
        ghci> :t g
        g :: (Foldable t, Num b) => t b -> b
        ghci> 6 == g [1..3]
        True
        ```
#### Manual currying and uncurrying

* Tuples can be used to manually represent uncurried functions. Here's a simple example of adding two variables represented in curried and uncurried forms.
    ```haskell
    ghci> f x y = x + y
    ghci> g (x,y) = x + y
    ghci> :t f
    f :: Num a => a -> a -> a
    ghci> :t g
    g :: Num a => (a, a) -> a
    ghci> g (1,2) == f 1 2
    True
    ```

#### Currying and uncurrying existing functions

* Tuples are also useful for uncurrying existing functions. Let's curry `fst`.
    ```haskell
    ghci> curry f x y = f (x, y)
    ghci> :t curry
    curry :: ((t1, t) -> t2) -> t1 -> t -> t2
    ghci> :t fst
    fst :: (a, b) -> a
    ghci> :t curry fst
    curry fst :: t2 -> b -> t2
    ghci> curry fst 1 2 == fst (1,2)
    True
    ```
* Now let's uncurry `(+)`:
    ```haskell
    ghci> uncurry f (x,y) = f x y
    ghci> :t uncurry
    uncurry :: (t2 -> t1 -> t) -> (t2, t1) -> t
    ghci> :t (+)
    (+) :: Num a => a -> a -> a
    ghci> :t uncurry (+)
    uncurry (+) :: Num t => (t, t) -> t
    ghci> uncurry (+) (1,2) == (+) 1 2
    True
    ```

#### Sectioning

* Infix operators can be partially applied. Key thing to remember here is some operators are commutative (`(+2) x == (2+) x` for any `x :: Num a`). Some are not (`(^2) x /= (^2)` for any `x >= 0` ). **NOTE**: `(^)` is non-negative integer exponentiation. Consider `(^^)` for full integer exponentation, or `(**)` for exponentiation over the reals.
* In any event, let's try some examples:
    ```haskell
    ghci> map (^2) [1..] /= map (2^) [0..]
    True
    ghci> (/2) 17 /= (2/) 17
    True

    -- This will go on further. Haskell supports infinite lists
    ghci> map (+2) [0..] == map (2+) [0..]
    ^CInterrupted.

    -- This evaluations almost immediately
    ghci> map (^2) [1..] /= map (2^) [0..]
    True

    -- Let's partially apply infixed map
    ghci> f = (`map` [1..10])
    ghci> :t f
    f :: (Num a, Enum a) => (a -> b) -> [b]
    ghci> f (+2)
    [3,4,5,6,7,8,9,10,11,12]
    ```
* **QUESTION**: Infix operations appear to be binary. Is this actually true? What does it mean to be infix on three or more operands?

### Polymorphism

* **Polymorphism** is te formulation of single expressions that can operate over multiple types adhering to certain conditions.
* Types come in three flavors:
    1. **concrete**: Restricted to a single form (i.e., `Int`, `Double`, `[Char]`)
    2. **polymorphically constrined**: Types are *constrained* by a [type class](../chapter06/README.md) (i.e., `Num a`, `Foldable t`). These class of types form the basis for **ad hoc polymorphism** in Haskell.
    3. **parametrically polymorphic**: Types that are unconstrained in their **type variables** (i.e., `(a,b)` or [a]).
* By convention, in Haskell **type variables** are lower-case and represented with single alpbabetic characters (occasionally, two characters--one alphabetic, one numeric \[i.e., `t0`\])
* **Type class constraints** and **concrete types** are always represented with initial capitalization.
* Let's consider some examples:
    * `id :: a -> a` is parametrically polymorphic. It is unconstrained in its type, and its operation, which is simply to return what is passed as an argument, requires no constraint to be apply to variables of any conceivable type.
    * `fst :: (a, b) -> a` is also parametrically polymorphic.
    * `map :: (a -> b) -> [a] -> [b]` is parametrically polymorphic.
    * `foldr ::  Foldable t => (a -> b -> b) -> b -> t a -> b` is polymorophically constrained in type class `Foldable`.
    * `foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b` is similarly constrained polymorphically.
* I don't know of any base functions in Haskell that are concrete. I get the feeling that it's best practice to at least shoot for constrained polymorphism wherever possible. After all, writing once is better than writing a lot. So here's a contrived example:
    ```haskell
    ghci> let intPlus :: Int -> Int -> Int; intPlus x y = x + y
    ghci> :t intPlus
    intPlus :: Int -> Int -> Int
    ghci> 2 `intPlus` 2
    4
    ghci> :t (2 `intPlus` 2)
    (2 `intPlus` 2) :: Int
    ```

#### Polymorphic constants

* Literals can be polymorphic:
    * `1` is **maximally polymorphic** (I smell [covariance/contravariance](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance) coming on) to `Num t => t`
    * `3.14159` is maximally polymorphic to `Fractional t => t` (`pi`, by the way, is maximally polymorphic to `Floating a => a`)
* The appropriate implementation of an operation on say, `1` and `3.14159` is selected by inferring the least maximally polymorphic argument and constraining all applicable terms to that type within the expression.
    ```haskell
    ghci> :t (-1 + 3.14159)
    (-1 + 3.14159) :: Fractional a => a
    ghci> :t ((-1::Double) + 3.14159)
    ((-1::Double) + 3.14159) :: Double
    ghci> :t ((-1::Double) + 3.14159::Float)

    <interactive>:1:2: error:
        • Couldn't match expected type ‘Float’ with actual type ‘Double’
        • In the expression: ((- 1 :: Double) + 3.14159 :: Float)
    ghci> :t ((-1::Int) + 3.14159::Float)

    <interactive>:1:2: error:
        • Couldn't match expected type ‘Float’ with actual type ‘Int’
        • In the expression: ((- 1 :: Int) + 3.14159 :: Float)
    ```


#### Working around constraints

* How do we employ the output of `length :: Foldable t => t a -> Int` as an operand in division (`(/) :: Fractional a => a -> a -> a`)? We employ a function that maximizes the polymorphicity of `Int` to `Num a => a`. This function is called `fromIntegral`.
    ```haskell
    ghci> :t fromIntegral
    fromIntegral :: (Num b, Integral a) => a -> b
    ghci> y = (1 / (fromIntegral $ length [0..10]))
    ghci> y
    9.090909090909091e-2
    ghci> :t y
    y :: Fractional a => a

    ```
* There are [a whole bunch of these](http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:fromInteger) operations that both lift numeric types up the polymorphic chain or back down the pipe. The function `fromInteger` itself is a [composition]()https://wiki.haskell.org/Function_composition) of two functions: [`fromInteger`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:fromInteger) and [`toInteger`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:toInteger) (see [source]()http://hackage.haskell.org/package/base-4.10.0.0/docs/src/GHC.Real.html#fromIntegral).

### Type inference

* Haskell employs [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) type inference to save us a lot of work. You've already seen a lot of this at work, and by now should have noticed we have not had to frequently (or at all at this point) supply type annotations.
    ```haskell
    ghci> f x y = x + y
    ghci> :t f
    f :: Num a => a -> a -> a
    ```
* GHC will infer the **maximally polymorphic** type(s) applicable to an operation's operands.
    ```
    ghci> f x y = x / y
    ghci> :t f
    f :: Fractional a => a -> a -> a

    ghci> g = f 1 (2.0::Double)
    ghci> :t g
    g :: Double

    ghci> h z = f (1.0::Double) z
    ghci> :t h
    h :: Double -> Double

    ghci> phi x = f (1.0::Fractional a => a) x
    ghci> :t phi
    phi :: Fractional a => a -> a
    ghci> phi 1
    1.0
    ghci> phi 55
    1.818181818181818e-2
    ```
* The operation of [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) is beyond the scope of this book and these notes, so won't dwell on this.

### Asserting types for declarations

* Inference is work for both compiler and programmer. We constrain types to aid man and machine.
* We can constrain types via:
    1. **type signature**:
        ```haskell
        ghci> let triple :: Int -> Int; triple x = 3 * x
        ghci> :t triple
        triple :: Int -> Int
        ```
    1. **type annotation**:
        ```haskell
        ghci> triple' x = (3::Int) * x
        ghci> :t triple'
        triple' :: Int -> Int

        ```
* GHC's inference engine can help us avoid mistakes in declaring and annotating types:
    ```haskell
    ghci> let f :: Int -> Int -> Int; f x y = x + y + "test"

    <interactive>:47:45: error:
        • Couldn't match expected type ‘Int’ with actual type ‘[Char]’
        • In the second argument of ‘(+)’, namely ‘"test"’
          In the expression: x + y + "test"
          In an equation for ‘f’: f x y = x + y + "test"

    ```

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
1. ["Classes and Instances"](https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Classes_and_instances), ["Classes and Types"](https://en.wikibooks.org/wiki/Haskell/Classes_and_types), ["Haskell Wikibook"](https://en.wikibooks.org/wiki/Haskell)
1. [The GHC Team](https://ghc.haskell.org/trac/ghc/wiki/TeamGHC), [7.6. Class and instances declarations](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#class-and-instances-declarations), [The Glorious Glasgow Haskell Compilation System User's Guide, Version 8.2.1](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
1. Lipovača, Miran, ["Types and Type Classes"](https://http://learnyouahaskell.com/types-and-typeclasses), [Learn You A Haskell for Great Good!](https://http://learnyouahaskell.com/)
1. Lipovača, Miran, ["Making Our Own Types and Typeclasses"](http://learnyouahaskell.com/making-our-own-types-and-typeclasses), [Learn You A Haskell for Great Good!](https://http://learnyouahaskell.com/)
1. Diehl, Stephen, ["Type Systems"](http://dev.stephendiehl.com/fun/type_systems.html), [Write You A Haskell](http://dev.stephendiehl.com/fun/index.html)

## Assorted problems
