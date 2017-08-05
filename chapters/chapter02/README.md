# Hello, Haskell!

## Preparing a Haskell development environment

Every OS is sacred.

* [OS X and macOS](PREP.MACOS.md)
* [Ubuntu](PREP.UBUNTU.md)

I mainly use [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) rather than interact with the platform directly. Chris and Julie put together a fine [megatutorial](https://www.youtube.com/watch?v=sRonIB8ZStw) (Youtube).

## Interacting with Haskell code

### Sidetrack: ghc

Before we dive into the REPL, let's just play with the compiler itself. "Hello World" is indicated.

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
