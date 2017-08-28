# Basic Datatypes

## Data Declarations

* Haskell data declarations consist of **type and data constructors**.
    * `data Foo = Bar | Buzz`
    * Not sure why **data constructors** are "constructors," but this whole setup smells like enums. Maybe there's a relation.
* Introduced to `deriving Show` in the [Mood Swing exercise](exercises/solutions.md#mood). This is convenient given we haven't learned about [instances]() yet.

## Numeric types

* Introduced to `Int` (fixed precision) and `Integer` (arbitrary precision).
    * Importing `GHC.Int` introduces a flood of other fixed precision types related to `Int`.
* `Fractional` covers
    * `Float` and `Double` [IEE 754](https://en.wikipedia.org/wiki/IEEE_754) primitives.
    * `Rational` and `Scientific` arbitrary precision representation of the reals.
* All numerics have **[instances]()** of **[typeclass]()** `Num` (both concepts remain to be introduced).
* Playing around with the `Int` primitives, noting:
    * [Two's complement](https://en.wikipedia.org/wiki/Two%27s_complement) fixed precision implementation:
    ```
    ghci> 127::Int8
    127
    ghci> 128::Int8

    <interactive>:16:1: warning: [-Woverflowed-literals]
        Literal 128 is out of the Int8 range -128..127
        If you are trying to write a large negative literal, use NegativeLiterals
    -128
    ghci> (127+10)::Int8
    -119
    ghci> (127+11)::Int8
    -118
    ghci> (127+1)::Int8
    -128
    ghci>
    ```
    * Bounds
    ```
    ghci> import GHC.Int
    ghci> minBound::Int8
    -128
    ghci> minBound::Int16
    -32768
    ghci> minBound::Int32
    -2147483648
    ghci> maxBound::Int32
    2147483647
    ghci> maxBound::Int16
    32767
    ghci> maxBound::Int8
    127
    ```

## Comparing values

* Introduced to two new typeclasses:
    * `Eq a`
    * `Ord a`
* References to **instances**, specifically the lack of instance implementing `Ord` for our [Mood](solutions.hs) datatype. We don't attempt to do anything about that here.
* Haskell's smart enough to infer intended Numerical type from raw number symbols and operational context, and throws errors when explicit or operational constraint violations obtain.
    * `6 / 3`: 6 and 3 are treated as `Fractional`, computes.
    * `6 / 3::Int`: 3 is explicitly cast as `Int`, does not compute.
    * `6 / length [1, 2, 3]`: `length [1, 2, 3]` evaluates to `Int`, does not compute.


# Conditional flow

Main take away here is there's no evaluation of `Num` to some sort of `Bool` as you might expect in some other languages. Chris's example is:

```
ghci> x = 0
ghci> if x * 100 == 0 then "adopt a dog" else "or a cat"
"adopt a dog"
ghci> if x * 100  then "adopt a dog" else "or a cat"

<interactive>:40:4: error:
    • No instance for (Num Bool) arising from a use of ‘*’
    • In the expression: x * 100
        In the expression: if x * 100 then "adopt a dog" else "or a cat"
        In an equation for ‘it’:
            it = if x * 100 then "adopt a dog" else "or a cat"
```

I personally think this is very sane in this day and age.

## Tuples

Not breaking much new ground here. I'll just leave this with the type signatures for `fst` and `snd` (self explanatory) and Chris's admonition not to go crazy with tuple size (keep it 5 or less, mmm'kay?).
```
ghci> :t fst
fst :: (a, b) -> a
ghci> :t snd
snd :: (a, b) -> b
```

## Lists

Lists are what you expect. Elements have the same type.

* Empty list: `[]`
* Strings are `[Char]`:
    ```
    ghci> x = "test"
    ghci> y = ['t','e','s','t']
    ghci> x
    "test"
    ghci> y
    "test"
    ```
* Concatenation works on lists of anything
    ```
    ghci> import Mood
    ghci> [Woot] ++ [Blah]
    [Woot,Blah]
    ```
* You can nest lists, provided types remain in the same domain.
