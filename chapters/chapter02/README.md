# Hello, Haskell!

## Preparing a Haskell development environment

Every OS is sacred.

* [OS X and macOS](PREP.MACOS.md)
* [Ubuntu](PREP.UBUNTU.md)

I mainly use [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) rather than interact with the platform directly. Chris and Julie put together a fine [megatutorial](https://www.youtube.com/watch?v=sRonIB8ZStw) (Youtube).

## Interacting with Haskell code

### Sidetrack: ghc

Before we dive into the REPL, let's stretch with the compiler and "Hello World."

```
$ mkdir -p chapters/chapter02/scratch
$ pushd chapters/chapter02/scratch
$ touch hello.hs
```

And the contents of [hello.hs](scratch/hello.hs):

```
module Main where

main :: IO ()
main = do
  putStrLn "Hello, world"
```  

Finally, let's compile [hello.hs](scratch/hello.hs)
```
$ ghc --make hello

... # alternatively

$ stack ghc hello.hs
```

### The REPL (ghci)

`ghci` is a Read-Eval-Print Loop, similar to [Scala REPL](http://docs.scala-lang.org/overviews/repl/overview.html), [Python Interpreter](https://docs.python.org/3/tutorial/interpreter.html), or [irb](http://ruby-doc.com/docs/ProgrammingRuby/html/irb.html). Aside from evaluating legal Haskell expressions. There are several commands (prefaced with a colon `:`) worth noting:
  * `:t <expression>`: type signature for a legal Haskell expression
    ```
    ghci> :t product
    product :: (Num a, Foldable t) => t a -> a
    ```
  * `:info <name>`: Information on named functions, typeclasses, and type constructors, including coordinates to definitions
  * `:l <path/to/some/haskell.hs>`: You can load source files into the REPL and muck around with functions and values defined and implemented therein.
    ```
    ghci> :l chapters/chapter02/scratch/hello.hs
    ghci> main
    "Hello, World"
    ```
  * `:! <shell stuff>`: shell out, i.e. `:! clear` behaves as it would in your shell.

These are just the highlights. Read 2.2 straight for more, and consult the [documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html).

### Understanding expressions

* "Everything in Haskell is an **expression** or **declaration**"
  * Expressions (try'em out):
    * `1`
    * `2**10` (exponentiation: i.e., `^` in other languages)
    * `(9 + 5) * 3`
    * `9 - 5`
    * `9 + (-5)`
    * This does not work: `9 - -5`
      ```
      ghci> 9 - -5

      <interactive>:7:1: error:
          Precedence parsing error
              cannot mix ‘-’ [infixl 6] and prefix `-' [infixl 6] in the same infix expression

      ```
* We are reacquainted to the term **reducible expression (redex)** (see [All about lambda](../chapter01/README.md)).
  * Normal forms:
    * `1 + 1 -> 2`
    * `(9 + 5) * 3 -> 42`
    * `(9 - 5) -> 4`

### Functions

## Additional reading

1. ["Using GHCI"](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html), [Glasgow Haskell Compiler User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html), v. 8.2.1


## Additional Tools

* [Hoogle](https://wiki.haskell.org/Hoogle). Particularly the command line tool.
  * Installation (Stack)
    ```
    $ stack install hoogle
    $ hoogle generate       # downloads the internet, go get coffee
    ```
  * From within `ghci`, try `ghci> :! hoogle --info find`. Great if you need to look up something either not in scope or documentation beyond what you can get with `:t <whatevs>` or `:info <stuff>`
* [Haskeline](https://hackage.haskell.org/package/haskeline). Yet another [readline](https://cnswww.cns.cwru.edu/php/chet/readline/rltop.html). I needed this to [properly map Home and End](https://stackoverflow.com/a/33727632) on a full size Apple keyboard.  
  * Find out the escape characters for keys you'd like to map.
    ```
    $ ghc -e getLine  # press the desired key *and* Enter immediately
                      # afterwards to get the control sequence.

    ...

    # Home
    ^[[1~
    "\ESC[1~"

    # End
    ^[[4~
    "\ESC[4~"
    ```
  * Here's how my haskeline config looks.
    ```
    keyseq: xterm-256color "\ESC[4~" end
    keyseq: xterm-256color "\ESC[1~" home
    ```
