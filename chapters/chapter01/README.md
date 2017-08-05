# All you need is Lambda

## Chapter Exercises

Solutions availables [here](exercises/solutions.md).

## What is functional programming?

* Functional programs combine **expressions**.
* **Expressions** consist of:
  * concrete values (string literals, numbers, etc.)
  * variables (let's avoid a strict definition for now; go with your intuition)
  * functions (operations of varying degrees of [arity](https://en.wikipedia.org/wiki/Arity))
* [Paul Hudak](http://www.cs.yale.edu/homes/hudak/CS201S08/lambda.pdf) presents an alternative formulation if the form of **terms**, which include :
  * `x` (variables)
  * `\x. e` (abstractions, think functions: `e` here is shorthand for an expression, not an **unbound term**)
  * `e1 e2` (applications, an expression applied to another--think function application)
* **Purity (referential transparency)**: Given an expression and complete specification of its terms, it will always evaluate to the same result.

## What is a function?

* From math, a **[function](https://en.wikipedia.org/wiki/Function_(mathematics))** maps the [domain](https://en.wikipedia.org/wiki/Domain_of_a_function) of inputs to a [codomain](https://en.wikipedia.org/wiki/Codomain) of outputs.
  * `f : X -> Y`
* Dwelling on this will lead me into a rabbit hole and I'll never get shit done, so read the book and just go with that for now.

## The structure of lambda terms

For the sake of my fingers, going to use `\` in place of `λ`.

* The **head**: `\` followed by a set of symbols (i.e., `x`, `xy`, `xyz` and so on) representing arguments
* The **body**: an expression with parameters bound to the argument symbols in the **head**.
* Together, the head and the body form a function known as a **[lambda expression](https://en.wikipedia.org/wiki/Lambda_calculus#Definition)**.
* Unbound parameters in the body are considered **free**:
  * `\x.z` : This function takes any argument `x` and returns `z`. `z` is a **free** parameter. Think of them as **constants** (I suspect it's actually more general than that, but it'll do for now). We'll get to the significance of these in [**Beta reduction**](#Beta reduction (β-reduction)) and [**Combinators**](#Combinators).

### Alpha equivalence (α-conversion)

* Simply put, [alpha equivalence](https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence) means the choice of symbols in the head does not matter. Provided the corresponding bound parameters in the body match those in the head, you can choose whatever symbols you like:
  * `\x.x == \y.y == \俺.俺` (α-conversion is not a scam; it's really just easy)
* More formally, α-conversion renames bound parameters in the body along with renaming symbol declaration in the head.

### Beta reduction (β-reduction)

* [β-reduction](https://en.wikipedia.org/wiki/Lambda_calculus#Beta_reduction) reduces an expression to its normal form (an expression which can be reduced no further) through application of functions to arguments. Application follows certain rules:
  *
* Applying function `(\x.t)` to term `s`, then `(\x.t) s -> t[x := s]` (where `t` represents some an expression that may or may not have bound paramaters)
  * `(\x.x)s -> x[x := s] -> s`
  * `(\x.z)s -> z[x := s] -> z`
  * `(\x.xz)s -> x[x := s] -> zs`
* Lambda expression apply to one argument at a time. Multiple arguments may be expressed like `(\xy.t)`, but we can recover the one argument rule through **[currying](https://en.wikipedia.org/wiki/Currying)**.
  * `(\xy.xy) 1 2 -> x[x := 1] -> (\y.1y) -> y[y := 2] -> 1 2`: This doesn't leave us with a meaningful result...
  * ...so:
    ```
    (\xy.xy)(\z.z) 1  = (\y.(\z.z)y) 1 # x[x := (\z.z)]
                      = (\z.z) 1       # y[y := 1]
                      = 1              # z[z := 1]
    ```
  * Chris actually gave us the term `(\z.a)` instead of `(\z.z)`. This would have resulted in a reduction to `a`.
  * Take `(\xy.xy) (\z.a) 1`
    ```
    (\xy.xy) (\z.a) 1 = (\x.(\y.xy)) (\z.a) 1 # curried
                      = (\y.(\z.a)y) 1        # [x := (\z.a)]
                      = (\z.a) 1              # [y := 1]
                      = a                     # [z := 1]
    ```
* Some more complicated examples from Chris:
  * `(\xy.xxy)(\x.xy)(\x.xz)`
  * `(\xyz.xz(yz))(\mn.m)(\p.p)`
    * Curry: `(\x.\y.\z.xz(yz))(\m.\n.m)(\p.p)` # curry
    * Pass first arg `(\m.\n.m)` to `(\x.\y.\z.xz(yz))` to yield:
      * `(\y.\z.(\m.\n.m)z(yz))(\p.p)`
    * Pass second arg `(\p.p)` to `(\y.\z.(\m.\n.m)z(yz))` to yield:
      * `(\z.(\m.\n.m) z (\p.p) z)`
    * From there:
      ```
      (\z.(\m.\n.m) z (\p.p) z) = (\z.(\n.z) ((\p.p) z)) # rename 'm' -> 'z'
                                = (\z.(\n.z) z )         # identity on 'z'
                                = (\z.z)                 # rename 'n' -> 'z'
      ```
  *
### η-conversion

Not covered in this chapter, but [worth examining](https://en.wikipedia.org/wiki/Lambda_calculus#.CE.B7-conversion) in the context of reduction.

## Evaluation is simplification

Our ultimate objective is getting an expression into **beta normal form**. That is a form in which the rules of application permit no further reduction.

## Combinators

* **[Combinators](https://en.wikipedia.org/wiki/Combinatory_logic)** are expressions with no free variables.
  * Identity is a combinator: `(\x.x)`
  * `\xy.x` is a combinator.
  * `\y.x` is a **not** a combinator. Has a free variable in `x`.
  * `\x.xz` is a **not** a combinator. Has a free variable in `z`.
  * The [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus) is obviously a combinator, and an interesting one:
    ```
    Y g = (\f.(\x.f(x x)) (\x.f(x x))) g  # 1. definition
        = (\x.g(x x)) (\x.g(x x))         # 2. [f := g]. Note this equality
        = g((\x.g(x x)) (\x.g(x x))       # 3. [x := (\x.g(x x))]
        = g (Y g)                         # 4. As per 2:
                                          #    Y g -> (\x.g(x x)) (\x.g(x x))
        ...

        = g ( ... g (Y g) ...)            # Continued application

    ```

## Divergence

* Some lambda expressions **diverge**; that is they never reach a beta normal form.
* This has a rich connection to termination in computation. Divergent expressions do not terminate.
* Some examples:
  * `(\x.xx)(\x.xx)`: Following the expression rule `[x := (\x.xx)]` simply yields `(\x.xx)(\x.xx)` again.
  * The [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus) also diverges


## Additional reading

1. ["Lamba Calculus"](https://en.wikipedia.org/wiki/Lambda_calculus), Wikipedia
1. ["Combinatory logic"](https://en.wikipedia.org/wiki/Combinatory_logic), Wikipedia
1. Hudak, Paul, ["A brief and information introduction to lambda calculus"](http://www.cs.yale.edu/homes/hudak/CS201S08/lambda.pdf). PDF
1. Rojas, Raul, ["A Tutorial Introduction to the Lambda Calculus"](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf). PDF
1. Lezama, A. S., ["The Lambda Calculus"](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-820-fundamentals-of-program-analysis-fall-2015/lecture-notes/MIT6_820F15_L02.pdf). Slides

## Assorted Problems
1. ["CMSC330 Spring 2001, Practice Problems 5"](http://www.cs.umd.edu/class/spring2011/cmsc330/prac5-solutions.pdf). PDF
1. ["Problem Set 1, 6.827, Multithreaded Parallelism"](http://ocw.alfaisal.edu/NR/rdonlyres/Electrical-Engineering-and-Computer-Science/6-827Multithreaded-Parallelism--Languages-and-CompilersFall2002/77E925A7-3CCB-42F2-9074-FF0A4019F039/0/ps1.pdf). PDF

## Tools

1. ["\ Calculus Interpreter"](https://people.eecs.berkeley.edu/~gongliang13/lambda/): I'm not quite confident this is implemented correctly, but useful on some basic sanity checks.
