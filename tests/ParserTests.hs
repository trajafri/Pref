{-# LANGUAGE OverloadedStrings #-}

module ParserTests
  ( parserTests
  )
where

import           Data.Either
import qualified Data.Text                     as T
import           Syntax.Exp
import           Pref
import           Test.Tasty
import           Test.Tasty.HUnit

expErrorMsg :: T.Text
expErrorMsg = " converted to exp incorrectly."

allExpTests :: [Assertion]
allExpTests =
  [ assertEqual (show $ test <> expErrorMsg) ex $ codeToAst test
  | (test, ex) <-
    [ ("a"                 , return [Id "a"])
    , ("(a)"               , return [App (Id "a") []])
    , ("(a a)"             , return [App (Id "a") [Id "a"]])
    , ("(lambda (a) a)"    , return [Lambda ["a"] (Id "a")])
    , ("(lambda (a) (a a))", return [Lambda ["a"] (App (Id "a") [Id "a"])])
    , ( "(let ((a) (b b)) (a a))"
      , return
        [ App
            (Id "let")
            [ App (App (Id "a") []) [App (Id "b") [Id "b"]]
            , App (Id "a")          [Id "a"]
            ]
        ]
      )
    , ( "(let ((a) (b b)) (a a)) (a)"
      , return
        [ App
          (Id "let")
          [App (App (Id "a") []) [App (Id "b") [Id "b"]], App (Id "a") [Id "a"]]
        , App (Id "a") []
        ]
      )
    , ( "(let ((a) (b b)) (a a)) (define a) (a a)"
      , return
        [ App
          (Id "let")
          [App (App (Id "a") []) [App (Id "b") [Id "b"]], App (Id "a") [Id "a"]]
        , App (Id "define") [Id "a"]
        , App (Id "a")      [Id "a"]
        ]
      )
    , ( "(let ((b b)) (a a)) (define a) (a a)"
      , return
        [ Let [("b", Id "b")] (App (Id "a") [Id "a"])
        , App (Id "define") [Id "a"]
        , App (Id "a")      [Id "a"]
        ]
      )
    , ("(a b c d)"           , return [App (Id "a") [Id "b", Id "c", Id "d"]])
    , ("(lambda (a b c d) e)", return [Lambda ["a", "b", "c", "d"] $ Id "e"])
    , ("(lambda () a)"       , return [Lambda [] $ Id "a"])
    , ( "(let ((a a) (b b) (c c) (d d)) e)"
      , return
        [ Let [("a", Id "a"), ("b", Id "b"), ("c", Id "c"), ("d", Id "d")]
              (Id "e")
        ]
      )
    , ( "(let ((x (add1 2))\
              \(y (sub1 x))\
              \(z (fact 5))) (+ x y z))"
      , return
        [ Let
              [ ("x", App (Id "add1") [NLiteral 2])
              , ("y", App (Id "sub1") [Id "x"])
              , ("z", App (Id "fact") [NLiteral 5])
              ]
            $ App (Id "+") [Id "x", Id "y", Id "z"]
        ]
      )
    ]
  ]

failureMsg :: T.Text
failureMsg = " did not trigger an error"

allFails :: [Assertion]
allFails =
  [ assertBool (show $ f <> failureMsg) (isLeft $ codeToAst f)
  | f <-
    [ "("
    , ")"
    , "(a"
    , "(a (a a)"
    , "(a (a) a"
    , "(("
    , "(()"
    , "(())("
    , "()"
    , "(lambda (()) a)"
    ]
  ]

parserTests :: TestTree
parserTests =
  testGroup "Parser tests"
    $  [ testCase ("exp-test " ++ show i) t
       | (i, t) <- zip [1 :: Int, 2 ..] allExpTests
       ]
    ++ [ testCase ("failure " ++ show i) f
       | (i, f) <- zip [1 :: Int, 2 ..] allFails
       ]
