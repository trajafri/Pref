module InterpTests
  ( interpTestList
  ) where

import Data.Map as M
import Exp
import Lexer
import Parser
import Pref
import Test.HUnit

defaultEnv = insert "empty" E empty

errorMsg = " interpreted incorrectly"

allTests =
  [ TestCase $ assertEqual (show test ++ errorMsg) (return [e]) (codeToVal test)
  | (e, test) <-
      [ (I 1, "1")
      , (S "a", "\"a\"")
      , (C "x" (NLiteral 2) defaultEnv, "(lambda (x) 2)")
      , (I 2, "((lambda (x) 2) 3)")
      , (I 2, "((lambda (x) 2) 3)")
      , (I 5, "((lambda (x y z) z) 3 4 5)")
      , (I 3, "(let ((x 1) (y 2) (z 3)) z)")
      , (I 2, "((lambda () 2))")
      , (S "HelloWorld", "(string-append \"Hello\" \"World\")")
      , (S "Hello World", "(string-append \"Hello\" \" \" \"World\")")
      , (I 5, "(define five 5) five")
      , (I 120, "(fix (lambda (fact x) (if x (* x (fact (- x 1))) 1)) 5)")
      , ( I 8
        , "(fix (lambda (fib last curr n)\
                          \ (if (- n 2) (fib curr (+ last curr) (- n 1)) curr)) 1 1 6)")
      , (I 42, "(car (cons (+ 30 12) empty))")
      , (Cons (I 42) E, "(cdr (cons 2 (cons (+ 30 12) empty)))")
      , ( (Cons (I 1) (Cons (I 2) (Cons (I 3) E)))
        , "(cons 1 (cons 2 (cons 3 empty)))")
      ]
  ]

interpTestList =
  TestList [TestLabel ("test " ++ show i) t | (i, t) <- zip [1,2 ..] allTests]
