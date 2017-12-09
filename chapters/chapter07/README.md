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

* First, let's draw a distinction between **argument** and **parameter**.
    * An **argument** is a value or function passed to a function when it is **called**.
    * A **parameter** is a specification of an argument when a function is **defined**.
* Haskell functions actually take only **one** argument, the result of which may be applied to additional arguments and so on ([currying](../chapter05/README.md#Currying)).
    * Haskell's syntactic sugar permits function definition with multiple parameters.
    * Partial application permits us to partially supply arguments (in the  left-outward direction).
    * The following illustrates a multiple parameter function definition `f`
    ```haskell
    ghci> f x y = x + y
    ghci> g x = (+) x
    ghci> f 2 2
    4
    ghci> g 2 2
    4
    ghci> h = g 2
    ghci> h 2
    4
    ```

## Anonymous functions

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
