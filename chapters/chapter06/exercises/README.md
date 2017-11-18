# Exercise Solutions

## Directions

* [Table of Contents](../../../README.md)
* Review the [notes here](../README.md).

## Eq Instances

Infer the `Eq` instance declaration for the supplied data types:

1. `data TisAnInteger`
    ```haskell
    data TisAnInteger = TisAn Integer

    instance Eq a => Eq (TisAn a) where
        (==) (TisAn Integer) (TisAn Integer) =
    ```
## Will they work?

Determine if the following `Ord` operations will work on the operands supplied.

1. `max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])`
    * Yes.
    ```haskell
    ghci> max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
    5
    ```
1. `compare (3 * 4) (3 * 5)`
    * Yes.
    ```haskell
    ghci> max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
    5
    ```
1. `compare "Julie" True`
    * No. Compare expects two arguments of the same type. `"Julie"` is `[Char]`, and `True` is `Bool`.
    ```haskell
    ghci> compare "Julie" True

    <interactive>:68:17: error:
        • Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
        • In the second argument of ‘compare’, namely ‘True’
          In the expression: compare "Julie" True
          In an equation for ‘it’: it = compare "Julie" True

    ```
1. `compare (3 * 4) (3 * 5)`
    * Yes.
    ```haskell
    ghci> (5 + 3) > (3 + 6)
    False
    ```

## Chapter Exercises

### Multiple Choice

1. The `Eq` class
    1. `c)` makes equality tests possible.
1. The type class `Ord`
    1. `b)` is a subclass of `Eq`. This is what the notation `class Eq a => Ord a` signifies
1. Suppose the typeclass `Ord` has an operator `>`. What is the type of `>`?
    1. `a) Ord a => a -> a -> Bool`
1. In `x = divMod 6 12`
    1. `c)` the type of `x` is a tuple.
1. The type class `Integral` includes
    1. `a)` Int and Integer numbers

### Does it typecheck?

1. Consider the following:
    ```haskell
    data Person = Person Bool

    printPerson :: Person -> IO ()
    printPerson person = putStrLn (show person)
    ```
    1. No. Data type `Person` has no instances for `Show`. Consider deriving it.
1. Consider the following:
    ```haskell
    data Mood = Blah | Woot deriving Show

    settleDown x = if x == Woot
                    then Blah
                    else x
    ```
    1. No. Data type `Mood` has no instances for `Eq`. Consider deriving it:
    1. Fix the above so that it type checks.
        ```haskell
        data Mood = Blah | Woot deriving (Show, Eq)

        settleDown x = if x == Woot
                        then Blah
                        else x
        ```
        1. Acceptable inputs to `settleDown` are a) `Woot` and b) `Blah` once fixed.
        1. `settleDown 9` will fail because its type is `settleDown :: Mood -> Mood`. `9` is type `Num a => a`.
        1. Comparison will fail because `Mood` has no instances for `Ord`. Consider deriving it (along with `Eq`), the absence of which will also produce an error.
            ```haskell
            data Mood = Blah | Woot deriving (Show, Eq, Ord)

            settleDown x = if x == Woot
                            then Blah
                            else x
            ```
    1. Does the following type check? If not,  then why not?
        ```haskell
        type Subject = String
        type Verb = String
        type Object = String

        data Sentence = Sentence Subject Verb Object

        s1 = Sentence "dogs" "drool"
        s2 = Sentence "Julie" "loves" "dogs"
        ```
        1. The above statements will evaluate without error. Furthermore, `s2` will evaluate properly. However, attempting to evaluate `s1` will result in an error:
            ```
            ghci> s1

            <interactive>:11:1: error:
                • No instance for (Show (Object -> Sentence))
                    arising from a use of ‘print’
                    (maybe you haven't applied a function to enough arguments?)
                • In a stmt of an interactive GHCi command: print it

            ```
            This is because s1 applied to only two of three arguments yields a function, still waiting for its application to an argument of type `Object`, and functions do not implement instances for type class `Show` ([see here](https://wiki.haskell.org/Show_instance_for_functions)).
            ```haskell
            ghci> s1 "lots"
            Sentence "dogs" "drool" "lots"
            ```

### Given a datatype declaration, what can we do?

Our data type declarations:
```haskell
data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)
```

1. `phew = Papu "chases" True`
    1. No. The `Papu` type constructor expects types `Rocks String` and `Yeah Bool`, not `[Char]` and `Bool`.
1. `truth = (Rocks "chomskydoz") (Yeah True)`
    1. Yes.
1. `equalityForall :: Papu -> Papu -> Bool; equalityForAll p p' = p == p'`
    1. Yes.
1. `equalityForall :: Papu -> Papu -> Bool; equalityForAll p p' = p > p'`
    1. No. We have not supplied instances for `Papu` within the type class `Ord`.

### Match the types

1. Substitute `i :: a` for the declaration in `i :: Num a => a; i = 1`.
    1. No. Supplying the literal `1` to `f`'s assignment requires adhering to a type class no less constrained than `Num a`.
        ```haskell
        ghci> i :: a ; i = 1

        <interactive>:2:14: error:
            • No instance for (Num a) arising from the literal ‘1’
              Possible fix:
                add (Num a) to the context of
                  the type signature for:
                    i :: a
            • In the expression: 1
              In an equation for ‘i’: i = 1
        ```
1. Subtitute `f :: Num a => a` for the declaration in `f :: Float; i = 1.0`
    1. No. Supplying the literal `1.0` requires adhering to a type class no less constrained than `Fractional a`.
1. Subtitute `f :: Fractional a => a` for the declaration in `f :: Float; i = 1.0`
    1. Yes. See above.
1. Subtitute `f :: RealFrac a => a` for the declaration in `f :: Float; i = 1.0`
    1. Yes. `Fractional a` is a superclass of `RealFrac a`
1. Subtitute `freud :: Ord a => a` for the declaration in `freud :: a -> a; freud x = x`
    1. Yes. `freud :: a -> a` is parametrically polymorphic. Adding a class constraint `Ord a` to the definition simply limits its application to values of type `Ord a`.
1. Subtitute `freud' :: Int -> Int` for the declaration in `freud' :: a -> a; freud' x = x`
    1. Yes. `freud' :: a -> a` is parametrically polymorphic. Adding a class constraint `Int` to the definition simply limits its application to values of type `Int`.
1. Substitute `a -> a` for the declaration in `sigmund :: Int -> Int; sigmund x = myX` where `myX = 1 :: Int`.
    1. No. The presence of `myX`, restricted to concrete type `Int`, and cannot be coerced into a parametrically polymorphic type.
1. Substitute `Num a => a -> a` for the declaration in `sigmund' :: Int -> Int; sigmund' x = myX` where `myX = 1 :: Int`.
    1. No. The presence of `myX`, restricted to concrete type `Int` requires that the function also meet this requirement, or `myX` be coerced via [`fromIntegral`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:fromIntegral).
1. Substitute `[Int] -> Int` for the declaration in `jung :: Ord a => [a] -> a; jung xs = head (sort xs)`
    1. Yes. `[Int] -> Int` merely constrains a parametrically polymorphic function to operate on a list of elements of type `Ord a`.
1. Substitute `Ord a => [a] - a` for the declaration in `young :: [Char] -> Char; young xs = head (sort xs)`
    1. Yes. The operations performed do not pin the function's type to a concrete instantiation. Indeed, the function could be maximally specified with `young :: [a] -> a`.
1. Substitute `Ord a => [a] -> a` for the declaration in `signifier :: [Char] -> Char; signifier xs = head (mySort xs)` where `mySort :: [Char] -> [Char]; mySort = sort`
    1. No. `mySort` pins the maximal type `signifier` can specify as `[Char] -> Char`. You must refactor `mySort` to support `Ord a => [a] -> [a]` in order to work.

### Type-Kwon-Do Two: Electric Typealoo

We all saw that coming.
