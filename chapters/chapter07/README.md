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

**Pattern matching** in functional and logic programming languages broadly consists of dispatching execution based on the bound parameters of function. Patterns are matched against **values** (or deconstructed components of values). Values include literals, user-defined data, and tuples and lists of the same.

* Envisioned as a control structure, pattern matching is an alternative to:
    1.  **if-then-else** and **if-then-else if...-else**.
    2. [case expressions](#case-expressions)
    3. [guards](#guards)
* Pattern matching against numbers. The `_` indicates "match anything, but don't bother binding parameter to a name."
    ```haskell
    isItZero :: Integer -> Bool
    isItZero 0 = True
    isItZero _ = False
    ```
* Pattern matching against numbers, this time binding parameters to names
    ```haskell
    -- Need to supply Num, Eq, and Ord to the signature for
    -- the compiler to deduce all the necessary types for function
    -- parameters to operate with a literal number to a maximally
    -- polymorphic degree.
    neverAddZero :: (Num a, Eq a, Ord a) => a -> a -> a
    neverAddZero 0 y = y
    neverAddZero x 0 = x
    neverAddZero x y = x + y
    ```
* Pattern matching against data constructors (using the [RegisteredUser example](scratch/registeredUser.hs))
    ```haskell

    newtype Username = Username String
    newtype AccountNumber = AccountNumber Integer

    data User = UnregisteredUser | RegisteredUser Username AccountNumber

    printUser :: User -> IO ()
    printUser UnregisteredUser = putStrLn "Unregistered"
    printUser (RegisteredUser (Username name) (AccountNumber acctNum))
        = putStrLn $ name ++ " " ++ show acctNum

    -- ...

    ghci> :l scratch/registeredUser.hs
    [1 of 1] Compiling RegisteredUser   ( scratch/registeredUser.hs, interpreted )
    Ok, modules loaded: RegisteredUser.
    ghci> printUser UnregisteredUser
    Unregistered
    ghci> printUser (RegisteredUser (Username "Prez") (AccountNumber 1322))
    Prez 1322
    ghci>

    ```
*  Pattern matching against data type constituents (using [penguins.hs](scratch/penguins.hs))
    ```haskell
    data WherePenguinsLive =
            Galapagos
        |   Antarctica
        |   Australia
        |   SouthAfrica
        |   SouthAmerica
        deriving (Eq, Show)

    data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

    -- Deconstructing a data type to get at its constituents
    gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
    gimmeWhereTheyLive (Peng whereItLives) = whereItLives

    -- ...

    ghci> gimmeWhereTheyLive (Peng Galapagos)
    Galapagos
    ghci> gimmeWhereTheyLive (Peng Antarctica)
    Antarctica



    -- ...

    -- Given a bunch of penguins, test if any are from Galapagos
    humboldt = Peng SouthAmerica
    gentoo = Peng Antarctica
    macaroni = Peng Antarctica
    little = Peng Australia
    galapagos = Peng Galapagos

    galapagosPenguin :: Penguin -> Bool
    galapagosPenguin (Peng Galapagos) = True
    galapagosPenguin _ = False

    -- ...

    ghci> antarcticOrGalapagos humboldt
    False
    ghci> antarcticOrGalapagos gentoo
    True
    ghci> antarcticOrGalapagos galapagos
    True
    ```
* Pattern matching against tuples.
    ```haskell
    fst' :: (a,b) -> a
    fst' (x,_) = x
    fst'' :: (a,b) -> a
    fst'' (a,b) = a

    first2 :: (a,b,c) -> (a,b)
    first2 (x,y,_) = (x,y)
    first2' :: (a,b,c) -> (a,b)
    first2' (a,b,c) = (a,b)
    --

    snd' :: (a,b) -> b
    snd' (_,y) = y
    snd'' :: (a,b) -> b
    snd'' (a,b) = b

    last2 :: (a,b,c) -> (b,c)
    last2 (_,x,y) = (x,y)
    last2' :: (a,b,c) -> (b,c)
    last2' (a,b,c) = (b,c)
    ```

## Case expressions

* **Case expressions** recall similar control structures in other languages (i.e., `switch` in C and Java, `case/esac` in Bash). Alternatives to case expressions include:
    1.  **if-then-else** and **if-then-else if...-else**.
    2. [pattern matching](#pattern-matching)
    3. [guards](#guards)
* Case expressions evaluate a single **expression** and match the resulting value to a [series of patterns](http://learnyouahaskell.com/syntax-in-functions#case-expressions), with the wildcard underscore operator `(_)` to match any expression in the exhaustive case.
    ```haskell
    case <expression> of <pattern> <- <result>
                         <pattern> <- <result>
                         <pattern> <- <result>
                         _         <- <result> -- the exhaustive case
    ```
    * Numbers:
    ```haskell

    -- Symbols `:{` and `:} are how we implement
    -- multiline expressions within ghci
    ghci> :{
        isZero x =
           case x of
               0 -> True
               _ -> False
    :}

    ghci> isZero 0
    True
    ghci> isZero 1
    False
    ghci> isZero 1.0
    False
    ghci> isZero 0.0
    True
    ```
    * Data types
    ```haskell
    ghci> :{
        printBool x =
           case x of
               True -> print "True"
               _ -> print "False"
        :}

    ghci> printBool True
    "True"
    ghci> printBool False
    "False"
    ```
    * Expressions
    ```haskell
    ghci> :{
        abs' x =
           case (x < 0) of -- (x < 0) is an expression
               True -> (-1) * x
               False -> x
    :}

    ghci> abs' (-1)
    1
    ghci> abs' 1
    1
    ghci> abs' 0
    0
    ```
    * Lists
    ```haskell
    ghci> :{
        head' l =
            case l of
                [] -> error "list is empty"
                (x:_) -> x

        tail' l =
            case l of
                [] -> error "list is empty"
                (_:xs) -> xs
    :}

    ghci> head' []
    *** Exception: list is empty
    CallStack (from HasCallStack):
      error, called at <interactive>:4:15 in interactive:Ghci1
    ghci> head' [1,2,3]
    1
    ghci> tail' []
    *** Exception: list is empty
    CallStack (from HasCallStack):
      error, called at <interactive>:9:15 in interactive:Ghci1
    ghci> tail' [1,2,3]
    [2,3]
    ```

## Higher order functions

Functions are first class citizens in Haskell. You can construct them [anonomously](#anonymous-functions), bind them to names, and bind them to the parameters of functions.

* Haskell provides several functions from the jump that should be familiar to folks coming from a functional programming background. Here are a couple of examples:
    1. `map`
        ```haskell
        ghci> f x = x^2
        ghci> map f [1,2,3,4,5]
        [1,4,9,16,25]
        ```
    1. `foldr`. We'll dive deeper into **folding lists** in [Chapter 10](../chapter10/README.md) (list comprehensions in [Chapter 9](../chapter09/README.md#list-comprehensions)).
        ```haskell
        ghci> f x y = x + y
        ghci> foldr g 0 [1,2,3,4,5]

        ghci> g x y = x * y
        ghci> foldr h 1 [1..5]
        120

        -- Let's try a lambda expression using prefix notation
        -- and a list comprehension
        ghci> foldl (\x y -> (/) y x) 1 [ x / 10 | x <- [1..10]]
        4.063492063492064
        ```

## Guards

* **Guards** resemble [piecewise functions](https://en.wikipedia.org/wiki/Piecewise). As control structures, guards are alternatives to:
    1.  **if-then-else** and **if-then-else if...-else**.
    2. [pattern matching](#pattern-matching)
    3. [case expresisons](#case-expressions)
* Like **if-then-else**, guards dispatch based on evaluation of **expressions**. Unlike [pattern matching](#pattern-matching) and [case expressions](#case-expressions), guards do not match values to patterns.

### Writing guard blocks

* The following function is written using **if-then-else** syntax
    ```haskell
    abs' :: Num a => a -> a
    abs' x = if x < 0 then (-x) else x
    ```
* Rewriting in guard syntax, we have
    ```haskell
    abs' :: Num a => a -> a
    abs' x
        | x < 0     = (-x)
        | otherwise = x
    ```
    The keyword `otherwise` serves a similar role to `(_)` in [pattern matching](#pattern-matching) and [case expressions](#case-expressions). If and once all other expressions evaluate to `False`, `otherwise` supplies an exhaustive evaluation to complete the operation.
* Guards can evaluate several expressions:
    ```haskell
    f :: Num a => a -> a
    f x
        | x < -1                    = x**2
        | 1 < x && x < exp(1)       = log(x)
        | exp(1) <= x || x < 2**32  = x**(1/4) - 3
        | otherwise                 = 1
    ```
* Guards can be used with `where` declarations
    ```haskell
    g x
        | y < 0     = -y
        | otherwise = y^2
        where
            y = x + 2
    ```
## Summary of control structures

Let's review what we've learned about:
1. **if-then-else**
1. [Pattern matching](#pattern-matching)
1. [Case expressions](#case-expression)
1. [Guards](#guards)

To that end, we'll implement a function to determine the nth value of a Fibonacci sequence defined openly on the integers (we'll review **recursion** in [Chapter 8](../chapter08/README.md)).

* **if-then-else** is a control pattern in various languages, functional or otherwise. We will not dwell on it except to note its verbosity ([fib-ifthen.hs](scratch/fib-ifthen.hs)).
    ```haskell
    module FibIfThen where

    fib' :: Integral a => a -> a
    fib' n =
        if (n == 0)
            then 1
            else if (n == 1)
                then 1
                else if (n == (-1))
                  then (-1)
                  else (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))
    ```
* [Pattern matching](#pattern-matching) dispatches based on the value of argument(s) bound to a function's parameters ([fib-pattern.hs](scratch/fib-pattern.hs)).
    ```haskell
    module FibPattern where

    fib' :: Integral a => a -> a
    fib' (-1)  = 1
    fib' 0     = 1
    fib' 1     = 1
    fib' n     = (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))
    ```
    The above example highlights an important limitation in matching patterns on values (thus far). Specifically, we have to match on exact values. The patterns supplied cannot be expressions. For example ([fib-pattern-expression.hs](scratch/fib-pattern-expression.hs)):
    ```haskell
    module FibPattern where

    fib' :: Integral a => a -> a
    fib' ((-4) + 3) = 1      -- we've subsituted (-1)
    fib' (1-1)      = 1      -- we've subsituted 0
    fib' ((-2) + 3) = 1      -- we've subsituted 1
    fib' n          = (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))
    ```
    Attempting to load or compile the above leads to the following error:
    ```haskell
    ghci> :l scratch/fib-pattern-expression.hs
    [1 of 1] Compiling FibPattern       ( scratch/fib-pattern-expression.hs, interpreted )

    scratch/fib-pattern-expression.hs:4:7: error:
        Parse error in pattern: (- 4) + 3
    Failed, modules loaded: none.
    ```
    This does not diminish the value of pattern matching, however. Decomposing values by their data constructors, or matching against specific constructions of lists and tuples is enormously useful.
* [Case expressions](#case-expressions) dispatch based on an *expressions* and a series of patterns, with (`_`) as a wildcard to match any remaining exhaustive cases ([fib-case.hs](scratch/fib-case.hs)):
    ```haskell
    module FibCase where

    fib' :: Integral a => a -> a
    fib' n =
      case (-1 <= n && n <= 1) of
        True  -> 1
        False -> (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))

    -- use the wildcard (_) pattern to match the exhaustive cases
    fib'' :: Integral a => a -> a
    fib'' n =
      case n of
        -1  -> 1
        0  -> 1
        1  -> 1
        _ -> (n `quot` abs(n)) * (fib' (abs(n)-1) + fib' (abs(n)-2))
    ```
* [Guards](#guards) dispatch based on multiple **expressions**. Guards resemble [piecewise functions](https://en.wikipedia.org/wiki/Piecewise) and operate similarly to  ([fib-guard.hs](scratch/fib-guard.hs)):
    ```haskell
    module FibGuard where

    fib' :: Integral a => a -> a
    fib' n
      | -1 <= n && n <= 1   = 1
      | otherwise           = (n `quot` abs(n)) * (fib' ((abs(n))-1) + fib' (abs(n)-2))


    -- a bit contrived in the second and exhaustive conditions
    -- to illustrate a three condition guard
    fib'' :: Integral a => a -> a
    fib'' n
      | -1 <= n && n <= 1   = 1
      | 1 < n               = (fib' (n-1) + fib' (n-2)) -- positive only Fibonacci
      | otherwise           = (-1) * (fib' ((abs(n))-1) + fib' (abs(n)-2))
    ```
    Note we were able to reduce the implementation of Fibonacci to 4 essential lines (including the signature). Guards fucking rock.

Additionally, there is the topic of [pattern guards](https://wiki.haskell.org/Pattern_guard) which we don't dive into right now.

## Function composition

**Function composition** is closely related in notation and procedure to mathematical form of [function application]() with [the same name](https://en.wikipedia.org/wiki/Function_composition). It is  [pointwise](https://en.wikipedia.org/wiki/Pointwise), a notion we'll encounter when we discuss [pointfree style](#pointfree-style).

* We can rewrite `f(g(x))` (or `f g x`) as `(f . g) x`, where `f` and `g` are functions applied to some argument `x`.
* Let's take a closer look at the  `(.)` operator:
    ```haskell
    ghci> :t (.)
    (.) :: (b -> c) -> (a -> b) -> a -> c
    --     |______|    |______|   |_|  |_|
    --         |           |       |    |
    --         f           g       x    result
    ```
    As stated above, `f` and `g` are functions applied, `x` is the argument
* To build familiarity, let's examine some operations in which function composition, along with some other techniques we've picked up along the way, produces concise, clear code by reducing the number of parathesis and chaining operations.
    - In conjunction with the precedence operation (`$`).
        ```haskell
        ghci> exp . product $ [0.1..1]

        -- compare and contrast with
        ghci> exp(product [0.1..1]) -- or
        ghci> (exp . product) [0.1..1]
        ```
    - Partially applied functions can be used to introduce additional arguments into the pipeline.
        ```haskell
        ghci> f (a,b) = b
        ghci> g = (\x y -> x + y/2)

        -- going to break character for a bit and play
        -- with a list comprehension
        ghci>  foldl g 0 . map f . take 5 $ [(a,b) | a <- [1..10], b <- [-10..0]]

        -- compare and contrast with
        ghci> foldl g 0 (map f (take 5 [(a,b) | a <- [1..10], b <- [-10..0]]))
        ```
    - A personal favorite of mine: constant time factorial.
        ```haskell
        import Math.Gamma -- you may need to install this with stack or cabal

        -- We'll implement this in an even more concise way in the next section
        ghci> f n = round . exp . lnGamma . fromIntegral  $ n + 1
        ghci> f 1
        1
        ghci> f 2
        2
        ghci> f 3
        6
        ghci> f 4
        24
        ```
    - Lambda expressions allows us to intercede in the composition of functions. Below is a highly contrived scenario demonstrating the possibility--not the wisdom--of this sort of programming. Given the obvious reasoning speed bump in this example, you should probably never do anything like this.
        ```haskell
        ghci> f x = exp(x)
        ghci> g x = log(x)
        ghci> g . f $ 1
        1.0
        ghci> f . g = \x -> f(g(x + 2))
        ghci> g . f $ 1
        3.0
        ghci> :r            -- useful for clearing the REPL of bound values
                            -- and functions
        ```

## Pointfree style

Pointfree style is a notational approach to expressing composition in **function definition** without reference to **arguments**. It is closely related to [pointwise operations](https://en.wikipedia.org/wiki/Pointwise) on functions, but not precisely the same thing.

* Trivially, pointfree style includes binding functions to new names
    ```haskell
    ghci> f x y = x + y
    ghci> f 2 3
    5
    ghci> f = (+)   -- defining function f without respect to arguments
    ghci> f 2 3
    5
    ```
* More interestingly, pointfree style is most often used to express a new function defined from the composition of others.
    ```haskell
    -- sum squares of integers
    ghci> f = sum . (map (^2))    -- parentheses about map (^2) to emphasize
                                  -- that it is a partially applied function
    ghci> f [1..10]
    385
    ```
* From the text, we've implemented the [`Arith2` module](scratch/arith2.hs) (with some slight renaming). Review and reason about the output.
    ```haskell
    module Arith2 where

    add :: Int -> Int -> Int
    add x y = x + y

    add' :: Int -> Int -> Int
    add = (+)

    inc :: Int -> Int
    inc = \x -> x + 1

    inc' :: Int -> Int
    inc' = (+1)

    main :: IO ()
    main = do
      print (0::Int)
      print (add 1 0)
      print (inc 0)
      print (inc' 0)
      print ((inc . inc) 0)
      print ((inc' . inc) 0)
      print ((inc . inc') 0)
      print (negate (inc 0))
      print ((negate . inc) 0)
      print ((inc . inc . inc . negate . inc) 0)
    ```

## Demonstrating composition

Problem. We would like to implement a function that prints a text representation of anything implementing `Show` to the screen. The canonical implication is [`print`](http://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:print), with a type signature of `Show a => a -> IO ()`. As explained in [Chapter 3](../chapter03/README.md), we can use two functions to implement this operation:
1. [`show`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:show), which takes a value of type `Show a => a` to `String`, and
1. [`putStrLn`](https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:putStrLn), which takes a type of `String` to `IO ()` (and hence to screen output).

* We previously implemented this in standard notation
    ```haskell
    print' :: Show a => a -> IO ()
    print' s = putStrLn (show s)
    ```
* In point free notation, this simple becomes:
    ```haskell
    print' :: Show a => a -> IO ()
    print' = putStrLn . show
    ```

## Additional reading

1. ["First-class function"](https://en.wikipedia.org/wiki/First-class_function), [Wikipedia](https://en.wikipedia.org)
1. ["Function composition"](https://en.wikipedia.org/wiki/Function_composition), [Wikipedia](https://en.wikipedia.org)
1. ["Pattern matching"](https://en.wikipedia.org/wiki/Pattern_matching), [Wikipedia](https://en.wikipedia.org)
1. ["Pattern Matching"](http://wiki.c2.com/?PatternMatching), [WikiWikiWeb](http://wiki.c2.com/?WikiWikiWeb)
1. ["Case Expressions and Pattern Matching"](https://www.haskell.org/tutorial/patterns.html), *[A Gentle Introduction to Haskell, Version 98](https://www.haskell.org/tutorial/index.html)*
1. ["Haskell/Pattern matching"](https://en.wikibooks.org/wiki/Haskell/Pattern_matching), [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)
1. ["Case Expressions"](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13), *[Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)*
1. ["Pattern guard"](https://wiki.haskell.org/Pattern_guard), [Haskell Wiki](https://wiki.haskell.org)
1. ["Function application"](https://en.wikipedia.org/wiki/Function_application), [Wikipedia](https://en.wikipedia.org)
1. ["Pointwise"](https://en.wikipedia.org/wiki/Pointwise), [Wikipedia](https://en.wikipedia.org)
1. ["Function composition"](https://en.wikipedia.org/wiki/Function_composition), [Wikipedia](https://en.wikipedia.org/)
