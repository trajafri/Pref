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
import           Test.HUnit

defaultEnv :: Map T.Text Val
defaultEnv = insert "empty" E empty

errorMsg :: T.Text
errorMsg = " interpreted incorrectly"

allTests :: [Test]
allTests =
  [ TestCase $ assertEqual
      (show $ testCase <> errorMsg)
      [e]
      (either (\_ -> []) (fromRight []) $ codeToVal testCase)
  | (testCase, e) <-
    [ ("1"                          , I 1)
    , ("\"a\""                      , S "a")
    , ("(lambda (x) 2)"             , C "x" (NLiteral 2) defaultEnv)
    , ("((lambda (x) 2) 3)"         , I 2)
    , ("((lambda (x) 2) 3)"         , I 2)
    , ("((lambda (x y z) z) 3 4 5)" , I 5)
    , ("(let ((x 1) (y 2) (z 3)) z)", I 3)
    , ("((lambda () 2))"            , I 2)
    , ("(string-append \"Hello\" \"World\")", S "HelloWorld")
    , ("(string-append \"Hello\" \" \" \"World\")", S "Hello World")
    , ("(define five 5) five"       , I 5)
    , ("(fix (lambda (fact x) (if x (* x (fact (- x 1))) 1)) 5)", I 120)
    , ( "(fix (lambda (fib last curr n)\
                          \ (if (- n 2) (fib curr (+ last curr) (- n 1)) curr)) 1 1 6)"
      , I 8
      )
    , ("(car (cons (+ 30 12) empty))"         , I 42)
    , ("(cdr (cons 2 (cons (+ 30 12) empty)))", Cons (I 42) E)
    , ( "(cons 1 (cons 2 (cons 3 empty)))"
      , Cons (I 1) (Cons (I 2) (Cons (I 3) E))
      )
    ]
  ]

interpTestList :: Test
interpTestList = TestList
  [ TestLabel ("test " ++ show i) t
  | (i, t) <- zip ([1, 2 ..] :: [Int]) allTests
  ]
