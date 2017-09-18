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

Files:
* [determine_the_type_p1.hs](determine_the_type_p1.hs)
* [determine_the_type_p2.hs](determine_the_type_p2.hs)
* [determine_the_type_p3.hs](determine_the_type_p3.hs)
* [determine_the_type_p4.hs](determine_the_type_p4.hs)
* [determine_the_type_p5.hs](determine_the_type_p5.hs)

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
1. Yes
1. No.
    ```haskell
    ghci> a = (+)
    ghci> b = 5
    ghci> c = b 10

    <interactive>:15:1: error:
        • Non type-variable argument in the constraint: Num (t -> t1)
          (Use FlexibleContexts to permit this)
        • When checking the inferred type
            c :: forall t t1. (Num (t -> t1), Num t) => t1

    ```
    Since `b` is type `Num t => t`, which is simply a value, we cannot apply it to `10` as we would a function. Instead, `a` makes a better candidate:
        ```haskell
        ghci> a = (+)
        ghci> b = 5
        ghci> c = a 10
        ghci> d = c 200

        ```
1. No. `b` is not in scope when `a` is declared, and `c` is never in scope.

### Type variable or specific type constructor?

Determine if the elements in the following type signatures are:
    1. Concretete
    2. Polymorphically constrained
    3. Parametrically polymorphic (unconstrained).

1. `f :: Num a => a -> b -> Int -> Int`
    ```haskell
         f :: Num a => a -> b -> Int -> Int
    --                |_|  |_|   |_|    |_|
    --                 |    |     |      |
    --                 1    2     3      4
    ```
    1. `a` is polymorphically constrained by the type class `Num`.
    1. `b` is parametrically polymorphic.
    1.  `Int` is concrete.
    1.  Same as above.
1. `f :: zed -> Zed -> Zed`
    ```haskell
         f :: zed -> Zed -> Zed
    --        |_|    |_|    |_|
    --         |      |      |
    --         1      2      3
    ```
    1. `zed` is parametrically polymorphic.
    1. `Zed` is concerete.
    1.  Same as above.
1. `f :: Enum b => a -> b -> c`
    ```haskell
         f :: Enum a => a -> b -> c
    --                 |_|  |_|  |_|
    --                  |    |    |
    --                  1    2    3
    ```
    1. `a` is polymorphically constrained by the type class `Enum`.
    1. `b` is parametrically polymorphic.
    1.  `c` is parametrically polymorphic.
1. `f :: f -> g -> c`
    ```haskell
         f :: f -> g -> C
    --       |_|  |_|  |_|
    --        |    |    |
    --        1    2    3
    ```
    1. `f` is parametrically polymorphic.
    1. `g` is parametrically polymorphic.
    1. `C` is concrete

### Write a type signature

Infer the type signature for the following expressions

1. `functionN (x:_) = x`
    * `functionN :: [a] -> a`
1. `functionC x y = if (x > y) then True else False`
    * `functionC :: Ord a => a -> a -> Bool`
1. `functionS (x, y) = y`
    * `functionS :: (a, b) -> b`

### Give a type, write the function

Infer the function given the type signature

1. `i :: a -> a`
    * `i x = x`
    * This is the identity function. It is the only function that satisfies this signature ([proof](https://stackoverflow.com/a/12230918/1375586)).
1. `c :: a -> b -> a`
    * `c x y = x`
1. `c'' :: b -> a -> b`
    * `c x y = x`
    * Under alpha equivalence, `c :: a -> b -> a` is the same as `c'' :: b -> a -> b`
1. `c' :: a -> b -> b`
    * `c x y = y`
1. `r :: [a] -> [a]`
    * Trivial: `r x = x`, an identity function restricted to lists
    * `f x = take 1 x`
    * `f x = reverse x`
1. `co :: (b -> c) -> (a -> b) -> a -> c`
    * `co g f x = g (f x)`, which can also be written
    * `co g f x = g $ f x`, and
    * `co g f x = (g . f) x`, and
    * `co g f = (g . f)`
    * The latter two are examples of [function composition](https://wiki.haskell.org/Function_composition), and the last one specifically in [point free notation](https://wiki.haskell.org/Pointfree)
1. `a :: (a -> c) -> a -> a`
    * `a f x = x`
    * The function `(a -> c)` is necessarily discarded as all arguments are parametrically polymorphic and we do not have an argument in `c` and the result is in `a`.
1. `a' :: (a -> b) -> a -> b`
    * `a f x = f x`, or
    * `a f = f` ([point free](https://wiki.haskell.org/Pointfree))

### Fix it

Objective is to fix some broken sources. I've done so and within their own source files.

1. Fix the following:
    ```haskell
    module sing where
    fstString :: [Char] ++ [Char]
    fstString x = x ++ " in the rain"

    sndString :: [Char] -> Char
    sndString x = x ++ " over the rainbow"

    sing = if (x > y) then fstString x or sndString y
    where x = "Singin"
       x = "Somewhere"
    ```
    * See [sing.hs](sing.hs) for the solution
1. Now that we've fixed above code, modify it so that it can print the other string. I've elected to do so in a fashion that will allow users of the `sing` function (renamed `sing'` to reinforce the difference in the reader's mind) to be switchable. See [sing_switchable.hs](sing_switchable.hs) for the solution
1. Fix the following:
    ```haskell
    -- arith3broken.hs
    module Arith3Broken where

    main :: IO () Main = do
        print 1 + 2
        putStrLn 10
        print (negate -1) print ((+) 0 blah) where blah = negate 1
    ```
    * See [arith3broken.hs](arith3broken.hs). Note, I changed the module name from `Arith3Broken` to `Main` so you can compile with `ghc` and run from the command line.


### Type-Kwon-Do

1. Supply the implementation for `???` in the following:
    ```haskell
    f :: Int -> String; f = undefined
    g :: String -> Char; g = undefined
    h :: Int -> Char; h = ???
    ```
    Solutions include:
        * `h x = g (f x)`
        * `h x = g $ f x`
        * `h x = (g . f) x`
        * `h = g . f`
1. Supply the implementation for `???` in the following:
    ```haskell
    data A
    data B
    data C
    q :: A -> B; q = undefined
    w :: B -> C;  w = undefined
    e :: A -> C; e = ???

    ```
    Solutions include:
        * `e x = w (q x)`
        * `e x = w $ q x`
        * `e x = (w . q) x`
        * `e = w . q`
1. Supply the implementation for `???` in the following:
    ```haskell
    data X
    data Y
    data Z
    xz :: X -> Z; xz = undefined
    yz :: Y -> Z; yz = undefined
    xform :: (X, Y) -> (Z, Z); xform = ???
    ```
    Solutions include
        * `xform (x,y) = ( xz x, yz y )`
        * `xform (x,y) = ( xz x, xz x )`
        * `xform (x,y) = ( yz y, yz y )`
1. Supply the implementation for `???` in the following:
    ```haskell
    munge :: (x -> y)
        -> (y -> (w, z))
        -> x
        -> w
    munge = ???

    ```
    * `munge f g x = (g . f) x`
