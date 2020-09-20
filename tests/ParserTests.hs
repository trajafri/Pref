{-# LANGUAGE OverloadedStrings #-}

module ParserTests
  ( parserTests
  )
where

import           Data.Either
import qualified Data.Text                     as T
import           Errors
import           Syntax.Exp
import           Lexer
import           Parser
import           Test.HUnit

-- These tests depend on Lexer.Tokenize
t1 :: T.Text
t1 = "a"

t2 :: T.Text
t2 = "()"

t3 :: T.Text
t3 = "(a)"

t4 :: T.Text
t4 = "(a a)"

t5 :: T.Text
t5 = "(lambda (a) a)"

t6 :: T.Text
t6 = "(lambda (a) (a a))"

t7 :: T.Text
t7 = "(let ((a) (b b)) (a a))"

t8 :: T.Text
t8 = "(let ((a) (b b)) (a a)) (a)"

t9 :: T.Text
t9 = "(let ((a) (b b)) (a a)) (define a) (a a)"

t10 :: T.Text
t10 = "(let ((b b)) (a a)) (define a) (a a)"

t11 :: T.Text
t11 = "(a b c d)"

t12 :: T.Text
t12 = "(lambda (a b c d) e)"

t13 :: T.Text
t13 = "(lambda () a)"

t14 :: T.Text
t14 = "(lambda (()) a)"

t15 :: T.Text
t15 = "(let ((a a) (b b) (c c) (d d)) e)"

t16 :: T.Text
t16 =
  "(let ((x (add1 2))\
              \(y (sub1 x))\
              \(z (fact 5))) (+ x y z))"

errorMsg :: T.Text
errorMsg = " parsed incorrectly"

allTests :: [Test]
allTests =
  [ TestCase $ assertEqual (show $ testCase <> errorMsg)
                           ex
                           (parse . fromRight [] . tokenize $ testCase)
  | (testCase, ex) <-
    [ (t1, return [Leaf "a"])
    , (t2, return [Node []])
    , (t3, return [Node [Leaf "a"]])
    , (t4, return [Node [Leaf "a", Leaf "a"]])
    , (t5, return [Node [Leaf "lambda", Node [Leaf "a"], Leaf "a"]])
    , ( t6
      , return
        [Node [Leaf "lambda", Node [Leaf "a"], Node [Leaf "a", Leaf "a"]]]
      )
    , ( t7
      , return
        [ Node
            [ Leaf "let"
            , Node [Node [Leaf "a"], Node [Leaf "b", Leaf "b"]]
            , Node [Leaf "a", Leaf "a"]
            ]
        ]
      )
    , ( t8
      , return
        [ Node
          [ Leaf "let"
          , Node [Node [Leaf "a"], Node [Leaf "b", Leaf "b"]]
          , Node [Leaf "a", Leaf "a"]
          ]
        , Node [Leaf "a"]
        ]
      )
    , ( t9
      , return
        [ Node
          [ Leaf "let"
          , Node [Node [Leaf "a"], Node [Leaf "b", Leaf "b"]]
          , Node [Leaf "a", Leaf "a"]
          ]
        , Node [Leaf "define", Leaf "a"]
        , Node [Leaf "a", Leaf "a"]
        ]
      )
    , ( t10
      , return
        [ Node
          [ Leaf "let"
          , Node [Node [Leaf "b", Leaf "b"]]
          , Node [Leaf "a", Leaf "a"]
          ]
        , Node [Leaf "define", Leaf "a"]
        , Node [Leaf "a", Leaf "a"]
        ]
      )
    , (t11, return [Node [Leaf "a", Leaf "b", Leaf "c", Leaf "d"]])
    , ( t12
      , return
        [ Node
            [ Leaf "lambda"
            , Node [Leaf "a", Leaf "b", Leaf "c", Leaf "d"]
            , Leaf "e"
            ]
        ]
      )
    , (t13, return [Node [Leaf "lambda", Node [], Leaf "a"]])
    , (t14, return [Node [Leaf "lambda", Node [Node []], Leaf "a"]])
    , ( t15
      , return
        [ Node
            [ Leaf "let"
            , Node
              [ Node [Leaf "a", Leaf "a"]
              , Node [Leaf "b", Leaf "b"]
              , Node [Leaf "c", Leaf "c"]
              , Node [Leaf "d", Leaf "d"]
              ]
            , Leaf "e"
            ]
        ]
      )
    , ( t16
      , return
        [ Node
            [ Leaf "let"
            , Node
              [ Node [Leaf "x", Node [Leaf "add1", Leaf "2"]]
              , Node [Leaf "y", Node [Leaf "sub1", Leaf "x"]]
              , Node [Leaf "z", Node [Leaf "fact", Leaf "5"]]
              ]
            , Node [Leaf "+", Leaf "x", Leaf "y", Leaf "z"]
            ]
        ]
      )
    ]
  ]

expErrorMsg :: T.Text
expErrorMsg = " converted to exp incorrectly."

allExpTests :: [Test]
allExpTests =
  [ TestCase $ assertEqual
      (show $ testCase <> expErrorMsg)
      ex
      (map treeToExp (fromRight [] $ tokenize testCase >>= parse))
  | (testCase, ex) <-
    [ (t1, [return $ Id "a"])
    , ( t2
      , [ Left $ ParseError
            "Found (). What does this mean?\nDid you mean to make a thunk?"
        ]
      )
    , (t3, [return $ App (Id "a") []])
    , (t4, [return $ App (Id "a") [Id "a"]])
    , (t5, [return $ Lambda ["a"] (Id "a")])
    , (t6, [return $ Lambda ["a"] (App (Id "a") [Id "a"])])
    , ( t7
      , [ return $ App
            (Id "let")
            [ App (App (Id "a") []) [App (Id "b") [Id "b"]]
            , App (Id "a")          [Id "a"]
            ]
        ]
      )
    , ( t8
      , [ return $ App
          (Id "let")
          [App (App (Id "a") []) [App (Id "b") [Id "b"]], App (Id "a") [Id "a"]]
        , return $ App (Id "a") []
        ]
      )
    , ( t9
      , [ return $ App
          (Id "let")
          [App (App (Id "a") []) [App (Id "b") [Id "b"]], App (Id "a") [Id "a"]]
        , return $ App (Id "define") [Id "a"]
        , return $ App (Id "a") [Id "a"]
        ]
      )
    , ( t10
      , [ return $ Let [("b", Id "b")] (App (Id "a") [Id "a"])
        , return $ App (Id "define") [Id "a"]
        , return $ App (Id "a") [Id "a"]
        ]
      )
    , (t11, [return $ App (Id "a") [Id "b", Id "c", Id "d"]])
    , (t12, [return $ Lambda ["a", "b", "c", "d"] $ Id "e"])
    , (t13, [return $ Lambda [] $ Id "a"])
    , ( t14
      , [ Left $ ParseError
            "Found (). What does this mean?\nDid you mean to make a thunk?"
        ]
      )
    , ( t15
      , [ return $ Let
            [("a", Id "a"), ("b", Id "b"), ("c", Id "c"), ("d", Id "d")]
            (Id "e")
        ]
      )
    , ( t16
      , [ return
          $ Let
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

allFails :: [Test]
allFails =
  [ TestCase $ assertBool
      (show $ f <> failureMsg)
      (case parse . fromRight [] . tokenize $ f of
        (Left  _) -> True
        (Right _) -> False
      )
  | f <- ["(", ")", "(a", "(a (a a)", "(a (a) a", "((", "(()", "(())("]
  ]

parserTests :: Test
parserTests =
  TestList
    $  [ TestLabel ("test " ++ show i) t
       | (i, t) <- zip [1 :: Int, 2 ..] allTests
       ]
    ++ [ TestLabel ("exp-test " ++ show i) t
       | (i, t) <- zip [1 :: Int, 2 ..] allExpTests
       ]
    ++ [ TestLabel ("failure " ++ show i) f
       | (i, f) <- zip [1 :: Int, 2 ..] allFails
       ]
