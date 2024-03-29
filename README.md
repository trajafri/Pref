![Pref CI](https://github.com/trajafri/Pref/workflows/Pref%20CI/badge.svg)
# Pref

(A simple project to get comfortable with my current Haskell dev environment, cabal etc.)

**Pref** is a lazy, lisp-like programming language

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
(also take a look at the .github CI/CD setup for running tests)

---

## Syntax/Grammar

* d ::= `(define x e)` 
* e ::= x  
     | (e e ...)  
     | (lambda (x ...) e)  
     | (let ([x e] ...) e)  
     | (if e e e)  
     | *empty*  
     | c
     | B
* x ::= *variable*
* c ::= integer | string | b
* b ::= #t | #f
* B ::= + | - | * | / | cons | car | cdr | empty? | zero? | string-append | fix | and | or | not

You can also use braces or brackets instead of parenthesis where needed.

### Variable

A variable identifier in Pref can be alphanumeric, as long as there is at least one
non-numeric character. It can also contain periods as long as there is at least one
non-numeric character. The following characters are not allowed:

* open/close parenthesis, braces, brackets, or angles
* single/double quotes
* comma, quasiquote (\`), `#`, `:`, or `;`

---

## Data Types

Pref comes with the following built-in data types:

* Integers
* Strings
* Booleans
* Lists

---

## `define`

The `define` grammar can be used to define top-level constants and functions.

For top-level functions, `define` is used as follows:

```(define f (lambda (x ...) e))```

Currently, `define` can only be used at the top level.

---

## TODOs

* Allow user to choose an evaluation strategy by passing in certain flags.
* Data Types: Allow user to define their own data type and (maybe) using pattern matching to work with the data.
* Refactor: Pref had a rough start which is why `define` can only be used at the top-level. This should be allowed where  
            a user expects to use `let`. 
* Error Locations: Both parser and the interpreter should indicate the location where the error occured.
* Compiler: compile Pref to x86.
* Infix Grammar: Try to use the same interpreter on an infix version of Pref (I guess it will be called *Infi* if that happens).
* Naive Macros: Compile time functions to manipulate the syntax tree.
