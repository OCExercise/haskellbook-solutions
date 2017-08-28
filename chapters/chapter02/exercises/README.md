# Exercise Solutions

## Parentheses and Associations

1. Evaluating the following two expressions yields different results as the parentheses surrounding the addition operation ensure it takes precedence over multiplication.
    1. `8 + 7 * 9` evaluates to `71`.
    1. `(8 + 7) * 9` evaluates to `135`.
1.The following two expressions are equivalent as the parentheses surround the multiplication sub-expressions which already take precedence over addition.
    1. `perimeter x y = (x * 2) + (x * 3)`
    1. `perimeter x y = x * 2 + x * 3`
1. The following two expressions **are not** are not equivalent as the parentheses surrounding the addition operation in the second ensures it takes precedence over division.
    1. `f x = x / 2 + 9`
    1. `f x = x / (2 + 9)` which is equivalent to `f x = x / 11`

## A Head Code

Evaluate the following:

1. `(let x = 5 in x) == 5`
1. `(let x = 5 in x * x) == 25`
1. `(let x = 5; y = 6 in x * y) == 30`
1. `(let x = 3; y = 1000 in x + 3) == 6`

Rewrite the following let expressions as expression with where bounds

1. `let x = 3; y = 1000 in x * 3 + y`
    1. `let f = x * 3 + y where x = 3; y = 1000`
1. `let y = 10; x = 10 * 5 + y in x * 5`
    1. `let f = x * 5 where y = 10; x = 10 * 5 + y`
1. `let x = 7; y = negate x; z = y * 10 in z / x + y`
    1. `let f = z / x + y where x = 7; y = negate x; z = y * 10`

## Chapter Exercises

### Parenthesization

Parenthesize the following expressions without changing the result:

1. `(2 + 2 * 3 - 1) == (2 + (2 * 3) - 1)`
1. `((^) 10 $ 1 + 1) == ((^) 10 $ (1 + 1))`
1. `(2 ^ 2 * 4 ^ 5 + 1) == (((2 ^ 2) * (4 ^ 5)) + 1)`

### Equivalent expressions

1. `(1 + 1) == 2`
1. `(10^2) == (10 + 9 * 10)`
1. `(400 - 37) /= ((-) 37 400)`
1. `(div 100 3) /= (100 / 3)` yields four `Ambiguous type` errors:
    ```
    <interactive>:44:1: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘/=’
    prevents the constraint ‘(Eq a0)’ from being solved.
    Probable fix: use a type annotation to specify what ‘a0’ should be.
    These potential instances exist:
    instance Eq Ordering -- Defined in ‘GHC.Classes’
    instance Eq Integer
    -- Defined in ‘integer-gmp-1.0.0.1:GHC.Integer.Type’
    instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Base’
    ...plus 22 others
    ...plus 9 instances involving out-of-scope types
    (use -fprint-potential-instances to see them all)
    • In the expression: (div 100 3) /= (100 / 3)
    In an equation for ‘it’: it = (div 100 3) /= (100 / 3)

    <interactive>:44:2: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘div’
    prevents the constraint ‘(Integral a0)’ from being solved.
    Probable fix: use a type annotation to specify what ‘a0’ should be.
    These potential instances exist:
    instance Integral Integer -- Defined in ‘GHC.Real’
    instance Integral Int -- Defined in ‘GHC.Real’
    instance Integral Word -- Defined in ‘GHC.Real’
    • In the first argument of ‘(/=)’, namely ‘(div 100 3)’
    In the expression: (div 100 3) /= (100 / 3)
    In an equation for ‘it’: it = (div 100 3) /= (100 / 3)

    <interactive>:44:6: error:
    • Ambiguous type variable ‘a0’ arising from the literal ‘100’
    prevents the constraint ‘(Num a0)’ from being solved.
    Probable fix: use a type annotation to specify what ‘a0’ should be.
    These potential instances exist:
    instance Num Integer -- Defined in ‘GHC.Num’
    instance Num Double -- Defined in ‘GHC.Float’
    instance Num Float -- Defined in ‘GHC.Float’
    ...plus two others
    ...plus one instance involving out-of-scope types
    (use -fprint-potential-instances to see them all)
    • In the first argument of ‘div’, namely ‘100’
    In the first argument of ‘(/=)’, namely ‘(div 100 3)’
    In the expression: (div 100 3) /= (100 / 3)

    <interactive>:44:17: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘/’
    prevents the constraint ‘(Fractional a0)’ from being solved.
    Probable fix: use a type annotation to specify what ‘a0’ should be.
    These potential instances exist:
    instance Fractional Double -- Defined in ‘GHC.Float’
    instance Fractional Float -- Defined in ‘GHC.Float’
    ...plus one instance involving out-of-scope types
    (use -fprint-potential-instances to see them all)
    • In the second argument of ‘(/=)’, namely ‘(100 / 3)’
    In the expression: (div 100 3) /= (100 / 3)
    In an equation for ‘it’: it = (div 100 3) /= (100 / 3)
    ```
1. `(2 * 5 + 18) /= 2 * (5 + 18)`

### More fun with functions

1. Given:
    ```
    z = 7
    y = z + 8
    x = y ^ 2
    waxOn = x * 5
    ```
    Evaluate the following:
    1. `10 + waxOn == 1135`
    1. `(+10) waxOn == 1135`
    1. `(-) 15 waxOn == -1110`
    1. `(-) waxOn 15 == 1110`
1. Given one, simply evaluate `let triple x = x * 3`
1. `waxOn == 1125`, so `triple waxOn == 3 * 1125`. `x` in function `triple x` is scoped to that function, and the previously defined `x` does not impact its value.
1. We can supply the givens as clauses to a `where` contained in `waxOn`'s definition (see [exercises.hs](exercises.hs)):
    ```
    waxOn     = x * 5
    where z = 7
    y = z + 8
    x = y^2
    ```
1. Simply added `triple x = x * 3` to the set of functions defined in [exercises.hs](exercises.hs).
1. Simply added `waxOff x = triple x` to the set of functions defined in [exercises.hs](exercises.hs).
1. Load [exercises.hs](exercises.hs) in the REPL. Go ahead and play around with `waxOff`. Change the source to make it do something different as the book suggests. Reload, rinse, wash, repeat.
