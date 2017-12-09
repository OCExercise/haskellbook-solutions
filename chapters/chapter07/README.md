# More functional patterns

## Directions

* [Table of Contents](../../README.md)
* Solutions availables [here](exercises/README.md).

## Make it func-y

* Like variables, functions can be bound to names. Like literals, they can be passed **[anonymously](#anonmyous-functions)**. Functions are ["first-class citizens"](https://en.wikipedia.org/wiki/First-class_function) in Haskell.
* We can **[match patterns](#pattern-matching)** against both variables and functions.
* We can define functions that consume other functions or produce functions as a result.
* We can partially apply functions to yield other functions
* We can compose functions in a manner closely related to the [mathematical sense](https://en.wikipedia.org/wiki/Function_composition).

## Arguments and parameters

* Let's draw a distinction between  **parameter** and **argument**.
    * **Parameters** specify inputs to a function and how they are used to produce a result.
        ```haskell
            f   x y  = x + y
        -- |_| |___|  |_____|
        --  |    |       |
        --  1    2       3

        ```
        1. `f` is the function name.
        1. `x` and `y` are parameters to the function `f`.
        1. Parameters `x` and `y` can now serve as operands to operations defined within within the body of the function.
    * **Arguments** are values (or functions) bound to parameters of a function in order to compute expressions therein to a final result.
        ```haskell
        ghci> f x y = x + y
        ghci> :t f
        f :: Num a => a -> a -> a

        -- Arguments are 2 and 3, passed to parameters x and y respectively
        ghci> f 2 3
        5

        -- Partially applied function
        ghci> g = (^2)


        -- Simple function composition
        ghci> h f = (+2) . f

        -- function g is argument to h, resulting in
        -- a function applied to value 2
        ghci> (h g) 2
        6
        ```

### Setting parameters


* You may think of values as functions with no parameters, as is evident from their types:
    ```haskell
    ghci> f = 1
    f :: Num t => t
    ghci> f x = 1
    ghci> :t f
    f :: Num t => t1 -> t

    ghci> f = 'a'
    ghci> :t f
    f :: Char
    ghci> f x = 'a'
    ghci> :t f
    f :: t -> Char
    ```
    * Values will always and only have a penultimate type (i.e., `t`, `Char`). Functions will always have at least one `->` symbol representing function application.
* Haskell functions actually take only **one** argument, the result of which may be applied to additional arguments and so on ([currying](../chapter05/README.md#Currying)).
    * Haskell's syntactic sugar permits function definition with multiple parameters.
    * Partial application permits us to partially supply arguments (in the  left-outward direction).
    * The following illustrates a multiple parameter function definition `f`
        ```haskell
        ghci> f x y = x + y
        ghci> g x = (+) x
        ghci> f 2 2
        4
        ghci> (f 2 2) == (g 2) 2    -- multiple argument function evaluates
        True                        -- equivalently to chain application
                                    -- of single argument functions
        ```

### Parametricity revisited

**NOTE**: This is an aside. Does not correspond to anything in the text. Hey, even I like to fiddle around a bit.

* Haskell infers the maximally polymorphic type of values or function within an expression unless directed otherwise. Below are examples aimed at cultivating intutition for this process.
    ```haskell
    -- maximally polymorphic in Num t
    ghci> a = 1
    ghci> :t a
    a :: Num t => t

    -- constrained to Integer
    ghci> a = 1 :: Integer
    ghci> :t a
    a :: Integer
    ```
* Parametrically polymorphic is as maximally polymorphic as Haskell can infer. Here, all parameters are parametrically polymorphic and can vary independently in type from one another.
    ```haskell
    ghci> g x y z = x
    ghci> :t g
    g :: t2 -> t1 -> t -> t2

    -- Same as above
    ghci> :t h
    h :: t2 -> t1 -> t -> (t2, t1, t)

    -- lists only consume elements of a single type, so parametrically
    -- polymorphic but all arguments must be of the same type
    ghci> l x y z = [x,y,z]
    ghci> :t l
    l :: t -> t -> t -> [t]

    -- parametrically polymorphic: x and y must be of the same
    -- type, but z is free to vary.
    ghci> p x y z = ([x,y],z)
    ghci> :t p
    lt :: t1 -> t1 -> t -> ([t1], t)

    -- z can vary, but while x and y can be parametrically polymorphic
    -- lists, they must be lists
    ghci> r x y z = ((head x), (head y), z)
    ghci> :t r
    r :: [t2] -> [t1] -> t -> (t2, t1, t)
    ```

### Binding variables to values

I'm going to approach this differently from the text and skip to the highlight. This will become readily apparent in some examples below in [anonmyous functions](#anonmyous-functions).

* **Binding** arguments to function is equivalent to [applying functions to arguments](../chapter01/README.md). Let's see how this works with explicit paranthesization.
    ```haskell
    ghci> f x y = x + y
    ghci> g = f(2)
    ghci> g(4)
    6
    ```
    1. We defined a function `f x y = x + y` (simple addition).
    2. We applied the function first to an argument `2` and bound the resulting function to the name `g`.
    3. We then applied `g` to argument `4` to get our final result.
* Let's segue to [anonmyous functions](#anonmyous-functions) with the following example:
    ```haskell
    ghci> ( (\x y -> x + y)(2) )(4)
    6
    ```

## Anonymous functions

* **Anonymous functions** (I prefer the term **lambda expressions**) are unbound expressions (contrast with [named expressions](), or what we've commonly called functions thus far.  Lambda expressions in Haskell bridge [lambda calculus](../chapter01/README.md) with Haskell syntax.
    ```haskell
    -- Given lambda expression for identity \x.x, in Haskell we have
    ghci> :t (\y -> y)
    (\y -> y) :: t -> t

    -- Application
    ghci> (\x -> x) "cat"
    "cat"
    ghci> (\x -> x) "1"
    "1"

    -- Alpha equivalence
    ghci> (\x -> x) (\y -> y) 18 "test"
    21

    -- Beta reduction
    ghci> (\x -> (\y -> x y)) (^2) 9
    81
    ```
* Multiple argument lambda expressions can be represented as sequentially applied single argument functions, just as they can be with lambda syntax.
    ```haskell

    -- \xy.x can be written \x.\y.x and vice versa
    ghci> (\x y -> x) 1 1 == (\x -> (\y -> x)) 1 1
    True
    ```
* Anonymous functions need not be bound to names and can be directly applied to arguments.
    ```haskell
    ghci> ( (\x y -> x + y)(2) )(4)
    6

    ghci> ( (\x y -> (++) x y)("cat") )(" in the hat")
    "cat in the hat"
    ```
* Nevertheless, lambda expressions can be bound to names.
    ```haskell
    ghci> add = (\x y -> x + y)
    ghci> 2 `add` 2
    4
    ```
* Lambda expressions can bind to parameters.
    ```haskell
    ghci> map (\x -> x^2) [0..5]
    [0,1,4,9,16,25]
    ```

### The utilty of lambda syntax

* Lambda expressions are great for
    - passing as one offs to [higher order functions]()
    - when you need to deviate from the evaluation procedure common to named expressions.

## Pattern matching

## Case expressions

## Higher order functions

## Guards

## Function composition

## Pointfree style

## Demonstrating composition


## Additional reading

1. ["First-class function"](https://en.wikipedia.org/wiki/First-class_function), [Wikipedia](https://en.wikipedia.org)
1. ["Function composition"](https://en.wikipedia.org/wiki/Function_composition), [Wikipedia](https://en.wikipedia.org)
