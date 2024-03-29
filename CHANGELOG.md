# Revision history for Pref

## 0.6.0.0 -- 2021-09-06

* Allow built-in functions to be used like normal functions
* Make `let` behave like Racket's `let` (i.e., binding `n`
  doesn't have binding `n-1` in scope)
* Remove n-ary operators (allows currying built-in operators)

## 0.5.0.0 -- 2020-06-10

* Make Pref call-by-need (Lazy)
* Remove EffectStack dependency
* Allow <, > in variable names

## 0.4.1.0 -- 2020-02-23

* Template Haskell support for Pref
* Support for writing top level definitions and Haskell expressions use Pref
* Add THExamples

## 0.4.0.0 -- 2019-09-04

* Pref now reads booleans
* If now expects booleans instead of 0 for failure
* Added `zero?`


## 0.3.4.1 -- 2019-08-30

* Pref now parses signed integers


## 0.3.4.0 -- 2019-08-24

* Made Pref call-by-name (non-eager)


## 0.3.3.0 -- 2019-08-23

* Added mutually recursive definitions


## 0.3.2.0 -- 2019-08-20

* Added recursive definitions
* Fixed a bug where parser expected an unneccessary expression
  at the end of a file


## 0.3.1.1 -- 2019-08-16

* Parse Text instead of String

## 0.3.1.0 -- 2019-08-14

* Add command line app for cps

## 0.3.0.0 -- 2019-07-08

* Hello Parsec!
* Replaced naive lexing/parsing with danq Parsec stuff
* Removed Syntax.Tokens and deleted tests that are taken care of by Parsec

## 0.2.1.1 -- 2019-07-06

* Use DList instead of regular list in Transfrom.CPS
* Add note in Transform.CPS regards its assumptions

## 0.2.1.1 -- 2019-07-06

* Remove ContT usage in Pref

## 0.2.1.0 -- 2019-07-06

* Use effect-stack in CPSer

## 0.2.0.0 -- 2019-07-05

* Replace String with Text

## 0.1.1.0 -- 2019-07-04

* Add CPSer and Pritty Printing

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
