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
* Case expressions can match on any data type, and case predicates can be expressions.
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

## Function composition

## Pointfree style

## Demonstrating composition


## Additional reading

1. ["First-class function"](https://en.wikipedia.org/wiki/First-class_function), [Wikipedia](https://en.wikipedia.org)
1. ["Function composition"](https://en.wikipedia.org/wiki/Function_composition), [Wikipedia](https://en.wikipedia.org)
1. ["Pattern matching"](https://en.wikipedia.org/wiki/Pattern_matching), [Wikipedia](https://en.wikipedia.org)
1. ["Pattern Matching"](http://wiki.c2.com/?PatternMatching), [WikiWikiWeb](http://wiki.c2.com/?WikiWikiWeb)
1. ["Case Expressions and Pattern Matching"](https://www.haskell.org/tutorial/patterns.html), *[A Gentle Introduction to Haskell, Version 98](https://www.haskell.org/tutorial/index.html)*
1. ["Haskell/Pattern matching"](https://en.wikibooks.org/wiki/Haskell/Pattern_matching), [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)
