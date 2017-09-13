# Exercise Solutions

## Type matching

Match the given operations to their type signatures:

1. `not :: Bool -> Bool`
1. `length :: [a] -> a`
1. `concat :: [[a]] -> [a]`. Note, this is an older definition. The newer and more general definition specifies `concat :: Foldable t => t a -> a`
1. `head :: [a] -> a`
1. `(<) :: (Ord a) => a -> a -> Bool`

## Type arguments

Long and short of it is, we can evaluate the types of functions as applied to arguments **without** defining either the function or the arguments. This is **not** a trick. It's math that I can't even explain right now.
```
ghci> let f :: a -> a -> a -> a; f = undefined
ghci> :t f
f :: a -> a -> a -> a
ghci> let x ::Char; x = undefined
ghci> f x
ghci> :t f x
f x :: Char -> Char -> Char

```
And that's pretty damned cool. Let's try some more examples:

1. What is the type of `f x` given `f :: a -> a -> a -> a` and:
    1. `x :: Char -> Char -> Char`?
        ```haskell
        ghci> let f :: a -> a -> a -> a; f = undefined
        ghci> let x :: Char -> Char -> Char; x = undefined
        ghci> :t f x
        f x
          :: (Char -> Char -> Char)
             -> (Char -> Char -> Char) -> Char -> Char -> Char

        ```
    1. `x :: x -> x -> x -> x`?
        ```haskell
        ghci> let x :: x -> x -> x -> x; x = undefined
        ghci> :t f x
        f x :: (x -> x -> x -> x) -> (x -> x -> x -> x) -> x -> x -> x -> x
        ```
    1. `x :: a -> a -> a`?
        ```haskell
        ghci> let x :: a -> a -> a; x = undefined
        ghci> :t f x
        f x :: (a -> a -> a) -> (a -> a -> a) -> a -> a -> a
        ```
    1. `x :: a -> a -> a -> Char`?
        ```haskell
        ghci> let x :: a -> a -> a -> Char; x = undefined
        ghci> :t f x
        f x
          :: (a -> a -> a -> Char)
             -> (a -> a -> a -> Char) -> a -> a -> a -> Char
        ```
1. If `g :: a -> b -> c -> b`, then the type of `g 0 'c' "woot"` is `Char`:
    ```haskell
    ghci> let g :: a -> b -> c -> b; g = undefined
    ghci> :t g 0 'c' "woot"
    g 0 'c' "woot" :: Char
    ```
1. If `h :: (Num a, Num b) => a -> b -> b`, then the type of `h 1.0 2"` is `Num b => b`:
    ```haskell
    ghci> let h :: (Num a, Num b) => a -> b -> b; h = undefined
    ghci> :t h 1.0 2
    h 1.0 2 :: Num b => b
    ```
    If you thought it might be `Integer`, consider that Haskell infers to the broadest possible type. While `2` might be an `Intenger`, it could also be a `Fractional`, or one of the reals. A natural number is always in the broadest category of `Num t`. Here are some other examples to illustrate:
        ```haskell
        ghci> :t h 1.0 2.0
        h 1.0 2.0 :: Fractional b => b

        ghci> :t h 1.0 sqrt(89)
        h 1.0 sqrt(89) :: (Num (a -> a), Floating a) => a
        ```
1. If `h :: (Num a, Num b) => a -> b -> b`, then the type of `h 1 (5 :: Double)` is `Double`:
  ```haskell
  ghci> :t h 1 (5 :: Double)
  h 1 (5 :: Double) :: Double
  ```
  In this case, we explicitly annotated the type supplanting `b` in the signature, thus constraining the result to `Double`.
1. If `jackal :: (Ord a, Eq b) => a -> b -> a`, then the type of `jackal "keyboard" "has the word jackal in it"` is `[Char]`:
    ```haskell
    ghci> let jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
    ghci> :t jackal "keyboard" "has the word jackal in it"
    jackal "keyboard" "has the word jackal in it" :: [Char]
    ```
1. If `jackal :: (Ord a, Eq b) => a -> b -> a`, then the type of `jackal "keyboard"` is `Eq b => b -> [Char]`:
    ```haskell
    ghci> let jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined

    -- Note the partial application of jackal, which evaluates
    -- across two operands, to its first argument
    ghci> :t jackal "keyboard"
    jackal "keyboard" :: Eq b => b -> [Char]
    ```
    See, we can evaluate the types of partially applied function even when they are undefined.
1. If `kessel :: (Ord a, Num b) => a -> b -> a`, then the type of `kessel 1 2` is **~~Ord a => a~~** (wrong!) `(Num a, Ord a) => a`:
    ```haskell
    ghci> let kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
    ghci> :t kessel 1 2
    kessel 1 2 :: (Num a, Ord a) => a
    ```
    **TODO**: I honestly thought this was `Ord a => a`, and I'm not sure why I'm wrong.
1. If `kessel :: (Ord a, Num b) => a -> b -> a`, then the type of `kessel (1 :: Integer) 2` is `Integer`:
    ```haskell
    ghci> :t kessel (1 :: Integer) 2
    kessel (1 :: Integer) 2 :: Integer
    ```
    **TODO**: This I got, but the previous exercise still trips me up. Gotta bug @bitemyapp about it.


## Parametricity

Parametric polymorphism is essentially an exercise in passing stuff around.

1. Chris and Julie assert that a function `f :: a -> a` can only be represented by the identity function. Let's prove it (with substantial help from [Gabriel Gonzalez](https://github.com/Gabriel439) in this [Stack Overflow answer](https://stackoverflow.com/a/12230918)):
    1. Not going to even try this now. Need to [learn more category theory](https://github.com/Gabriel439).
1. `f :: a -> a -> a` has only two implementations. Again, I won't bother to prove this now (because I don't know how yet), but:
    ```
    ghci> let g :: a -> a -> a; g x y = y
    ghci> let f :: a -> a -> a; f x y = x
    ghci> let g :: a -> a -> a; g x y = y
    ghci> :t f
    f :: a -> a -> a
    ghci> :t g
    g :: a -> a -> a
    ghci> (f 1 2) /= (g 1 2)
    True
    ```
    Essentially, curried `fst` and `snd`  with first and second arguments constrained to be the same type.
1. Finally, we see `f :: a -> b -> b`. This is curried `snd`: `f x y = y`, with `x` and `y` free to be any type, same or otherwise. The behavior clearly changes for `f :: a -> b -> a`, giving us curried `fst`:
    ```haskell
    ghci> :t curry fst
    curry fst :: t2 -> b -> t2
    ghci> :t curry snd
    curry snd :: a -> t2 -> t2
    ghci> :t curry
    ```

## Apply yourself

You are the compiler. Infer, bitch.

1. Given `(++) :: [a] -> [a] -> [a]`, infer the signature of `myConcat x = x ++ " yo"`
    * **Answer**: `[Char] -> [Char]`
1. Given `(*) :: Num a => a -> a -> a`, infer the signature of `myMult x = (x / 3) * 5`
    * **Answer**: `Fractional a => a -> a`
    * **Notes**: This is a trick question. We also need to know the signature of `(/)`, which is `Fractional a => a -> a -> a`. This is the polymorhpically more restrictive operation, hence the result.
1. Given `(>) :: Ord a => a -> a -> Bool`, infer the signature of `myCom x = x > (length [1..10])`
    * **Answer**: `Int -> Bool`
1. Given `(<) :: Ord a => a -> a -> Bool`, infer the signature of `myAlph x = x < 'z'`
    * **Answer**: `Char -> Bool`

## Chapter Exercises

### Multiple choice

1. A value of type `[a]` is:
    * **c)** a list in which all elements of are of type `a`
1. A function of type `[[a]] -> [a]` could:
    * **a)** take a list of strings as an argunment
1. A function of type `[a] -> Int -> a`
    * **b)** returns one element of type 𝑎 from a list
1. A value of type `(a,b) -> a` is:
    * **c)** takes a tuple as an argument and returns the first value

### Determine the type

As suggested in the text, we will use the `{-# LANGUAGE NoMonomorphismRestriction #-}` [pragma](http://downloads.haskell.org/~ghc/6.8.1/docs/html/users_guide/pragmas.html) to ensure top level declarations take maximally polymorphic types.

1. Determine the type of the following:
    1. `(* 9) 6` -> `Num a => a`
    1. `head [(0,"doge"),(1,"kitteh")]` -> `Num a => (a,b)`
    1. `head [(0 :: Integer ,"doge"),(1,"kitteh")]` -> `(Integer,[Char])`
    1. `if False then True else False` -> `Bool`
    1. `length [1, 2, 3, 4, 5]` -> `Int`
    1. `(length [1, 2, 3, 4]) > (length "TACOCAT")` -> `Bool`
1. Given
    ```haskell
    x = 5
    y = x + 5
    w = y * 10
    ```
    What is the type of `w`?
        * **Answer**: `Num a => a`
        * **Notes**: `(+)` and `(*)` are maximally polymorphic to `Num a => a -> a -> a`
1. Given
    ```haskell
    x = 5
    y = x + 5
    z y = y * 10
    ```
    What is the type of `z`?
        * **Answer**: `Num a => a -> a`
        * **Notes**: Same deal with `(+)` and `(*)`, however this time `z` is supplied an argument
1. Given
    ```haskell
    x = 5
    y = x + 5
    f = 4 / y
    ```
    What is the type of `z`?
        * **Answer**: `Fractional a => a`
        * **Notes**: `(/)` is polymorphically more restrictive than `(+)`, and since its operands are limited to `Fractional a => a`, all dependant functions must also take operands no more maximally typed than `Fractional a => a`
1. Given
    ```Haskell
    x = "Julie"
    y = " <3 "
    z = "Haskell"
    f = x ++ y ++ z
    ```
    What is the type of `f`?
        * **Answer**: `[Char]`


### Does it compile?

Answer yes if the following compile, or no and a fix if it doesn't.

1. No.
    ```haskell
    ghci> bigNum = (^) 5 $ 10
    ghci> wahoo = bigNum $ 10

    <interactive>:22:1: error:
        • Non type-variable argument in the constraint: Num (t -> t1)
          (Use FlexibleContexts to permit this)
        • When checking the inferred type
            wahoo :: forall t t1. (Num (t -> t1), Num t) => t1

    ```

### Type variable or specific type constructor?

### Write a type signature

### Give a type, write the function

### Fix it

### Type-Kwon-Do
