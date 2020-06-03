![Pref CI](https://github.com/trajafri/Pref/workflows/Pref%20CI/badge.svg)
# Pref

(A simple project to get comfortable with my current Haskell dev environment, cabal etc.)

---

## Features

**Pref** is an eager, lisp-like programming language with the following features:

* Higher Order Functions
* S-Expressions
* Local Variable Bindings
* Top Level Recursive Functions

---

## Syntax/Grammar

* d ::= `(define x e)` 
* e ::= x  
     | (e e ...)  
     | (lambda (x ...) e)  
     | (let ([x e] ...) e)  
     | (if e e e)  
     | (b e ...)  
     | *empty*  
     | c
* x ::= *variable*
* B ::= + | - | * | \ | cons | car | cdr | empty? | zero? | string-append | fix
* c ::= integer | string | b
* b ::= #t | #f

You can also use braces or brackets instead of parenthesis where needed.

### Variable

A variable identifier in Pref can be alphanumeric, as long as there is at least one
non-numeric character. It can also contain periods as long as there is at least one
non-numeric character. The following characters are not allowed:

* open/close parenthesis, braces, brackets, or angles
* single/double quotes
* comma, '#', '\`', ':', or ';'

---

## Data Types

Pref comes with the following built-in data types:

* Integers
* Strings
* Booleans
* Lists

---

## `define`

The `define` grammar can be used to define top-level contants and functions.

For top-level functions, `define` is used as follows:

```(define f (lambda (x ...) e))```

Currently, `define` can only be used at the top level.

---

## Usage

### Installing Haskell & Cabal

To use Pref, you need to have a minimal Haskell dev-environment installed.

On \*nix systems, checkout [ghcup](https://www.haskell.org/ghcup/) to install both Haskell and Cabal

### Interpreting Pref programs

With `cabal-install` installed, you can execute the following commands to evaluate a Pref program:

```
> cabal build all
> cabal exec pref *path*
```

where *path* points to a file containing a Pref program

---

## TODOs

* Higher Order Operators: Improve the interpreter so operators like `+`, `fix` etc can be treated like data.
* Lazy Evaluation.
* Allow user to choose an evaluation strategy by passing in certain flags.
* Data Types: Allow user to define their own data type and (maybe) using pattern matching to work with the data.
* Tag interesting points: For people interested, different versions of Pref will be tagged to see different approaches  
                          taken during this project. For example, viewers can check Pref when it didn't use Parsec, or  
                          when it wasn't lazy.
* Refactor: Pref had a rough start which is why `define` can only be used at the top-level. This should be allowed where  
            a user expects to use `let`. 
* Error Locations: Both parser and the interpreter should indicate the location where the error occured.
* Compiler: compile Pref to x86.
* Infix Grammar: Try to use the same interpreter on an infix version of Pref (I guess it will be called *Infi* if that happens).
* Naive Macros: Compile time functions to manipulate the syntax tree.
