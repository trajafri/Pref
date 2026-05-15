{-# LANGUAGE OverloadedStrings #-}

module ParserTests
  ( parserTests,
  )
where

import Data.Either
import Data.List.NonEmpty (NonEmpty ((:|)), singleton)
import qualified Data.Text as T
import Pref
import Syntax.Exp
import Test.Tasty
import Test.Tasty.HUnit

expErrorMsg :: T.Text
expErrorMsg = " converted to exp incorrectly."

allExpTests :: [Assertion]
allExpTests =
  [ assertEqual (show $ test <> expErrorMsg) ex $ codeToAst test
    | (test, ex) <-
        [ ("a", return [Id . Var $ "a"]),
          ("(a)", return [App (Id . Var $ "a") []]),
          ("(a a)", return [App (Id . Var $ "a") [Id . Var $ "a"]]),
          ("(lambda (a) a)", return [Lambda [Var "a"] [Id . Var $ "a"]]),
          ("(lambda (a) (a a))", return [Lambda [Var "a"] [App (Id . Var $ "a") [Id . Var $ "a"]]]),
          ( "(let ((a) (b b)) (a a))",
            return
              [ App
                  (Id . Var $ "let")
                  [ App (App (Id . Var $ "a") []) [App (Id . Var $ "b") [Id . Var $ "b"]],
                    App (Id . Var $ "a") [Id . Var $ "a"]
                  ]
              ]
          ),
          ( "(let ((a) (b b)) (a a)) (a)",
            return
              [ App
                  (Id . Var $ "let")
                  [App (App (Id . Var $ "a") []) [App (Id . Var $ "b") [Id . Var $ "b"]], App (Id . Var $ "a") [Id . Var $ "a"]],
                App (Id . Var $ "a") []
              ]
          ),
          ( "(let ((a) (b b)) (a a)) (define a) (a a)",
            return
              [ App
                  (Id . Var $ "let")
                  [App (App (Id . Var $ "a") []) [App (Id . Var $ "b") [Id . Var $ "b"]], App (Id . Var $ "a") [Id . Var $ "a"]],
                App (Id . Var $ "define") [Id . Var $ "a"],
                App (Id . Var $ "a") [Id . Var $ "a"]
              ]
          ),
          ( "(let ((b b)) (a a)) (define a) (a a)",
            return
              [ Let (singleton (Var "b", Id . Var $ "b")) [App (Id . Var $ "a") [Id . Var $ "a"]],
                App (Id . Var $ "define") [Id . Var $ "a"],
                App (Id . Var $ "a") [Id . Var $ "a"]
              ]
          ),
          ("(a b c d)", return [App (Id . Var $ "a") [Id . Var $ "b", Id . Var $ "c", Id . Var $ "d"]]),
          ("(lambda (a b c d) e)", return [Lambda [Var "a", Var "b", Var "c", Var "d"] [Id . Var $ "e"]]),
          ("(lambda () a)", return [Lambda [] [Id . Var $ "a"]]),
          ( "(let ((a a) (b b) (c c) (d d)) e)",
            return
              [ Let
                  ((Var "a", Id . Var $ "a") :| [(Var "b", Id . Var $ "b"), (Var "c", Id . Var $ "c"), (Var "d", Id . Var $ "d")])
                  [Id . Var $ "e"]
              ]
          ),
          ( "(let ((x (add1 2))\
            \(y (sub1 x))\
            \(z (fact 5))) (+ x y z))",
            return
              [ Let
                  ( (Var "x", App (Id . Var $ "add1") [NLiteral 2])
                      :| [ (Var "y", App (Id . Var $ "sub1") [Id . Var $ "x"]),
                           (Var "z", App (Id . Var $ "fact") [NLiteral 5])
                         ]
                  )
                  [App (Id . Var $ "+") [Id . Var $ "x", Id . Var $ "y", Id . Var $ "z"]]
              ]
          ),
          ("1", return [NLiteral 1]),
          ("2", return [NLiteral 2]),
          ("451", return [NLiteral 451]),
          ("\"hello!\"", return [SLiteral "hello!"]),
          ("\"hello world!\"", return [SLiteral "hello world!"]),
          ("-10", return [NLiteral (-10)]),
          ("#f", return [BLiteral False]),
          ("#t", return [BLiteral True]),
          ( "(if #t #t #f)",
            return [If (BLiteral True) (BLiteral True) (BLiteral False)]
          )
        ]
  ]

failureMsg :: T.Text
failureMsg = " did not trigger an error"

allFails :: [Assertion]
allFails =
  [ assertBool (show $ f <> failureMsg) (isLeft $ codeToAst f)
    | f <-
        [ "(",
          ")",
          "(a",
          "(a (a a)",
          "(a (a) a",
          "((",
          "(()",
          "(())(",
          "()",
          "(lambda (()) a)"
        ]
  ]

parserTests :: TestTree
parserTests =
  testGroup "Parser tests" $
    [ testCase ("exp-test " ++ show i) t
      | (i, t) <- zip [1 :: Int, 2 ..] allExpTests
    ]
      ++ [ testCase ("failure " ++ show i) f
           | (i, f) <- zip [1 :: Int, 2 ..] allFails
         ]
