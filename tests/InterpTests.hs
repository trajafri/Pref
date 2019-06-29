module InterpTests
  ( interpTestList
  ) where

import Data.Map as M
import Exp
import Lexer
import Parser
import Pref
import Test.HUnit

errorMsg = " interpreted incorrectly"

allTests =
  [ TestCase $ assertEqual (show test ++ errorMsg) (return [e]) (codeToVal test)
  | (e, test) <-
      [ (I 1, "1")
      , (S "a", "\"a\"")
      , (C "x" (NLiteral 2) empty, "(lambda (x) 2)")
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
      ]
  ]

interpTestList =
  TestList [TestLabel ("test " ++ show i) t | (i, t) <- zip [1,2 ..] allTests]
