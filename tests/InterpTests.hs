{-# LANGUAGE OverloadedStrings #-}

module InterpTests
  ( interpTestList
  )
where

import           Data.Either
import           Data.Map                      as M
import qualified Data.Text                     as T
                                         hiding ( zip )
import           Syntax.Exp
import           Pref
import           Test.Tasty
import           Test.Tasty.HUnit

defaultEnv :: Env
defaultEnv = Env $ insert "empty" (Empty, Env empty) empty

errorMsg :: T.Text
errorMsg = " interpreted incorrectly"

allTests :: [Assertion]
allTests =
  [ assertEqual (show $ test <> errorMsg)
                [e]
                (either (\_ -> []) (fromRight []) $ codeToVal test)
  | (test, e) <-
    [ ("1"                          , I 1)
    , ("\"a\""                      , S "a")
    , ("(lambda (x) 2)"             , C "x" (NLiteral 2) defaultEnv)
    , ("((lambda (x) 2) 3)"         , I 2)
    , ("((lambda (x) x) 3)"         , I 3)
    , ("((lambda (x y z) z) 3 4 5)" , I 5)
    , ("(let ((x 1) (y 2) (z 3)) z)", I 3)
    , ("((lambda () 2))"            , I 2)
    , ("(string-append \"Hello\" \"World\")", S "HelloWorld")
    , ("(string-append \"Hello\" \" \" \"World\")", S "Hello World")
    , ("(define five 5) five"       , I 5)
    , ("(fix (lambda (fact x) (if x 1 (* x (fact (- x 1))))) 5)", I 120)
    , ( "(fix (lambda (fib last curr n)\
                          \ (if (- n 2) curr (fib curr (+ last curr) (- n 1)))) 1 1 6)"
      , I 8
      )
    , ("(car (cons (+ 30 12) empty))"         , I 42)
    , ("(cdr (cons 2 (cons (+ 30 12) empty)))", Cons (I 42) E)
    , ( "(cons 1 (cons 2 (cons 3 empty)))"
      , Cons (I 1) (Cons (I 2) (Cons (I 3) E))
      )
    ]
  ]

interpTestList :: TestTree
interpTestList = testGroup
  "Interpreter tests"
  [ testCase ("test " ++ show i) t
  | (i, t) <- zip ([1, 2 ..] :: [Int]) allTests
  ]
