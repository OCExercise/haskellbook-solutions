# Strings

## Directions

* [Table of Contents](../../README.md)
* Solutions are available [here](exercises/README.md)

## Overview

This chapter is a sneaky introduction to **lists** , **types** and even the `main` loop. A `String` is a list of `Char`. Prove it to yourself in `ghci`:
```haskell
ghci> :info String
type String = [Char]    -- Defined in ‘GHC.Base’
```
As you can see, `String` is syntactic sugar for `[Char]`. Actually, it's a bit more than that, but we're not up to [algebraic data types](../chapter11/README.md) yet so this will suffice.

## A first look at types

* There's a nifty `:type` command in `ghci`. It works like so:
    ```haskell
    ghci> :type 'a'
    'a' :: Char

    # See? Strings are just lists of characters
    ghci> :type "Hello"
    "Hello" :: [Char]
    ```
* There's `print` function that works just great on strings, characters and a whole host of things we won't get into now.
    ```haskell
    ghci> print "Hello"
    "Hello"
    ```
    * `print` is far more useful in an actual source file than in `ghci` for reasons having to do with the [Show typeclass]() we won't get into right now.
* There's `putStrLn` which, unlike `print`, only accepts Strings.
    ```haskell
    ghci> :t putStrLn
    putStrLn :: String -> IO ()
    ghci> putStrLn "Hello"
    Hello
    ```
* If we create and load the following program into `ghci` and then compile and run it from the command line we can observe two interesting properties:
    ```haskell
    -- exercises/print1.hs
    module Main where
    main :: IO ()
    main = putStrLn "hello world!"
    ```
    1. `main` has a type `IO`
        ```
        ghci> :t main
        main :: IO ()
        ```
        We haven't gotten to [monads]() yet (hold your horses), so just consider this to be a very special sort of type for now.
    1. The function `main` in a [module]() named `Main` is the primary entry point for Haskell programs when compiled.
        ```
        $ ghc -o print1 print1.hs
        $ ./print1
        hello world!
        ```
        * **NOTE**: The text actually lists the program as such:
            ```
            -- exercises/print1.hs
            module Print1 where
            main :: IO ()
            main = putStrLn "hello world!"
            ```
            This will not compile for reasons that we'll go into when we touch on [modular]() and [project]() structure.
            ```
            $  ghc -o print1 print1.hs
            [1 of 1] Compiling Print1           ( print1.hs, print1.o )

            <no location info>: error:
                output was redirected with -o, but no output will be generated
            because there is no Main module.
            ```
* We'll often want to perform several operations within the `main` loop. Here's how:
    ```haskell
    -- exercises/print2.hs
    -- Again, the text titled this module "Print2," which will not compile

    module Main where
    main :: IO ()
    main = do
        putStrLn "Count to four for me:"
        putStr "one, two"
        putStr ", three, and"
        putStrLn " four!"
    ```
    * `do` is "syntactic sugar for [monadic]() expressions" ([source](https://wiki.haskell.org/Keywords#do)). For now, just consider this magic for specifying several operations in a block.

## String concatenation

* This is the meat of the stringy stuff touched upon in this chapter.
* **Concatenation** means to join things together. We've encountered this briefly in [Chapter 2](../chapter02/README.md) with the `(++)` operator and `concat` functions:
    ```haskell
    ghci> (++) "hello " "world"
    "hello world"

    ghci> "hello" ++ " world"
    "hello world"

    ghci> concat ["hello", " ", "world"]
    "hello world"

    ```
* We're again given an example program with which to play:
    ```haskell
    -- exercises/print3.hs
    -- Again, the text titled this module "Print3," which will not compile
    module Main where

    myGreeting :: String
    myGreeting = "hello" ++ " world!"

    hello :: String
    hello = "hello"

    world :: String
    world = "world!"

    main :: IO ()
    main = do
        putStrLn myGreeting
        putStrLn secondGreeting
        where secondGreeting =
        concat [hello, " ", world]
    ```
    * This program reintroduces very simple **type signatures**. In this case, the function `myGreeting` and the values `hello` and `world` are given explicit type signatures of type `String`. `main` of course retains its type of `IO`.

## Top-level versus local definitions

* This section concerns scope.
* **Top-level** refers to values and functions defined in the most global scope of a program.
* **Local** refers to values and functions defined within the scope of nesting declarations.
* Consider the following:
    ```haskell
    -- exercises/top_or_local.hs
    module TopOrLocal where

    topLevelFunction :: Integer -> Integer
    topLevelFunction x   = x + woot + topLevelValue
              where woot :: Integer
                    woot = 10

    topLevelValue :: Integer
    topLevelValue = 5
    ```
    * `topLevelFunction` and `topLevelValue` are both "top-level," or "globally scoped."
    * `woot` is "local" to `topLevelFunction` That is `woot`'s scope is bound to `topLevelFunction` and is not resolvable outside of it.

## Types of concatenation functions

* Revisiting `(++)` and `concat`, we examine their types
    ```haskell
    ghci> :type (++)
    (++) :: [a] -> [a] -> [a]

    ghci> :type concat
    concat :: Foldable t => t [a] -> [a]
    ```
* `(++)` operates on two arguments. `concat` operates on a list of arbitrary size containing elements (in this case, strings) of the same type.
    * **NOTE**: The text actually expects `concat :: [[a]] -> [a]`. This is an older formulation of the function. We will not get into [Foldable]() at this time.
* Reading a type signature is critical to grasping Haskell.  Take the following:
    ```haskell
    ghci> :type (++)
    (++) :: [a] -> [a] -> [a]
    ```
    * On the left-most side, we have the function itself: `(++)`.
    * The separate `::` delimits the function name from its signature
    * `[a]` represents a list of some unknown type `a` [applicable]() to this function (learn more about [type variables]()).
    * The `->` symbols represent function application to a single argument.
    * Read from left to right, the function is
        1. applied to the first `[a]`, it's result
        2. applied to the second `[a]`, yielding
        3. the final `[a]` as a result.
    * Expressed another way, `(++)` is a function on two list arguments yielding a list containing elements of the same type (i.e., `f([a], [a])` maps to `[a]`).
    * The [type variable]() `a` is critical to several types of [polymorphism]() permitted by Haskell. We won't delve into those right now.
    * Recall `String` a synonym of `[Char]`. Since `(++)` and `concat` operate on lists unconstrained in their element type `a`, they are applicable even to `[Char]` and thus `String`.

## Concatenation and scoping

* Consider the following program:
    ```haskell
    -- exercises/print3flipped.hs
    module Main where

    myGreeting :: String
    myGreeting = (++) "hello" " world"

    hello :: String
    hello = "hello"

    world :: String
    world = "world!"

    main :: IO ()
    main = do
        putStrLn myGreeting
        putStrLn secondGreeting
        where secondGreeting = (++) hello ((++) " " world)

    ```
    * The function `myGreeting` and values `hello` and `world` are defined globally.
    * The function `secondGreeting` is bound locally to `main`.
    * `(++)` is applied parenthatically to the literal `" "` and globally defined `world` before an additional `(++)` operation append the result to `hello`.
* Let's take a look at a broken program and see if we can figure out what's wrong with it. This exercise will explore scoping.
    ```haskell
    -- exercises/print3broken.hs
    module Main where

    printSecond :: IO ()
    printSecond = do
        putStrLn greeting

    main :: IO ()
    main = do
        putStrLn greeting
        printSecond
        where greeting = "Yarrrrr"
    ```
    * Attempting to load this in the REPL will result in the following error:
        ```haskell
        ghci> :l scratch/print3broken.hs
        [1 of 1] Compiling Main             ( scratch/print3broken.hs, interpreted )

        scratch/print3broken.hs:6:14: error:
            Variable not in scope: greeting :: String
        ```
    * The problem lies in the attempt to evaluate `greeting` within the scope of `printSecond`. `greeting` is only bound within the where clause of `main`.
    * The error message is quite lucid; giving the programmer the line and column number of the error as well as a clean and understandable message to help resolve the issue. Haskell compiler checking and error handling are two of the greatest strengths of the language and toolkit.
    * We can fix this by removing `greeting` to the global scope, or by adding a where clause to `printSecond` that scopes a different `greeting` value. The law of Do Not Repeat Yourself abhors the second solution, so:
        ```haskell
        -- exercises/print3fixed.hs
        module Main where

        printSecond :: IO ()
        printSecond = do
            putStrLn greeting

        main :: IO ()
        main = do
            putStrLn greeting
            printSecond
            where greeting = "Yarrrrr"
        ```

## More list functions

* For the nth time, `String` is `[Char]` and is therefore a list. We can use list operations on values of this type.
* The **cons** operation prepends an element to a list. Note that the element we're prepending is of type `Char`, not type `[Char]`. A `Char` is an element of the list `[Char]`, so it's only naturally we prepend the element. This is a key difference between `:` and `concat` or `(++)`.
    * `'c' : "hris" == "chris"`
    * `'p' : "rez" == "prez"`
* We can extract the `head` of a list: `head "chris" == 'c'`
* We can extract the `tail` of a list: `tail "chris" == "hris"`
* We can `take` a sublist of length `n` from the start of another, including nothing but no more than its entire contents:
    * `take 0 "Papuchon" == ""`
    * `take 1 "Papuchon" == "P"`
    * `take 2 "Papuchon" == "Pa"`
    * `take 8 "Papuchon" == "Papuchon"`
    * `take 20 "Papuchon" == "Papuchon"`
* We can drop `n` elements out of a list, including none but no more than all.
    * `drop 0 "Papuchon" == "Papuchon"`
    * `drop 1 "Papuchon" == "apuchon"`
    * `drop 2 "Papuchon" == "puchon"`
    * `drop 8 "Papuchon" == ""`
    * `drop 20 "Papuchon" == ""`
* We can extract an element from a list given its index (starting from 0)
    * `"Papuchon" !! 0 == 'P'`
    * `"Papuchon" !! 7 == 'n'`
    * But attempting to extract an element given an out of bound index results in an error:
        ```haskell
        ghci> "Papuchon" !! (-1)
        *** Exception: Prelude.!!: negative index
        ghci> "Papuchon" !! 8
        *** Exception: Prelude.!!: index too large
        ```
