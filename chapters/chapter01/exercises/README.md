# Exercise Solutions

For the sake of my fingers, I'll be using `\` in place of `λ`.

## Intermission

Choose the equivalent expression

1. `\xy.xz`
      * **b)**: `\mn.mz`. Alpha equivalence
1. `\xy.xxy`
      * **c)**: `\a(\b.aab)`. Alpha equivalence and currying (`\xy.t == \x.\y.t`).
1. `\xyz.zx`
      * **b)** `\tos.st`. Alpha equivalence

## Chapter Exercises

### Combinators

Which if the following are combinators?

1. `\x.xxx`: Combinator.
1. `\xy.zx`: Not a combinator. Free variable in `z`
1. `\xyz.xy(zx)`: Combinator.
1. `\xyz.xy(zxy)`: Combinator.
1. `\xy.xy(zxy)`: Not a combinator. Free variable in `z`

### Normal form of divergence

1. `\x.xxx`: Normal form. Cannot reduce any further
1. `(\z.zz)(\y.yy)`: Diverges:
    ```
        (\z.zz)(\y.yy) = (\y.yy)(\y.yy) # by α-conversion or [z := (\y.yy)]
    ```
1. `(\x.xxx)z`: Reducible to `zzz`, which is normal form.

# Beta reduce

1. `(\abc.cba)zz(\wv.w)`
    ```
    (\abc.cba)zz(\wv.w) = (\a.(\b.(\c.cba)))zz(\w.(\v.w)) # currying
                        = (\b.(\c.cbz))z(\w.(\v.w))       # [a := z]
                        = (\c.czz)(\w.(\v.w))             # [b := z]
                        = (\w.(\v.w))zz                   # [c := (\w.(\v.w))]
                        = (\v.z)z                         # [w := z]
                        = z                               # [v := z]
    ```
1. `(\x.(\y.xyy))(\a.a)b`
    ```
    (\x.(\y.xyy))(\a.a)b = (\y.(\a.a)y) y b # [x := (\a.a)]
                        = (\a.a) b b       # [y := b]
                        = b b              # [a := b]
    ```
1. `(\y.y)(\x.xx)(\z.zq)`
    ```
    (\y.y)(\x.xx)(\z.zq) = (\x.xx)(\z.zq)  # (\y.y) is identity
                        = (\z.zq)(\z.zq)  # [x := (\z.zq)]
                        = (\z.zq) q       # [z := (\z.zq)]
                        = q q             # [z := q]
    ```
1. `(\z.z)(\z.zz)(\z.zy)` # Hint. α-conversion
    ```
    (\z.z)(\z.zz)(\z.zy) = (\z.zz)(\z.zy) # (\z.z) is identity
                        = (\z.zy)(\z.zy) # [z := (\z.zy)]
                        = (\z.zy) y      # [z := (\z.zy)]
                        = y y            # [z := y]
    ```
1. `(\x.(\y.xyy)) (\y.y) y`
    ```
    (\x.(\y.xyy)) (\y.y) y = (\y.(\y.y)yy) y  # [x := (\y.y)]
                            = ((\y.y) y) y     # [y := y]
                            = y y              # [y := y]
    ```
1. `(\a.aa)(\b.ba)c`
    ```
    (\a.aa)(\b.ba)c = (\b.ba)(\b.ba)c  # [a := (\b.ba)]
                    = (\b.ba) a c      # [b := (\b.ba)]
                    = a a c            # [b := a]
    ```
1. `(\xyz.xz(yz))(\x.z)(\x.a)`
    ```
    (\xyz.xz(yz))(\x.z)(\x.a) = (\x.\y.\z.xz(yz)) (\x.z) (\x.a) # currying
                                = (\y.\z.(\x.z)z(yz)) (\x.a)      # [x := (\x.z)]

    # To be continued

    ```
