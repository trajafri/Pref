module ParserTests
  ( parserTests
  )
where

import           Data.Either
import           Errors
import           Syntax.Exp
import           Lexer
import           Parser
import           Test.HUnit

-- These tests depend on Lexer.Tokenize
t1 :: String
t1 = "a"

t2 :: String
t2 = "()"

t3 :: String
t3 = "(a)"

t4 :: String
t4 = "(a a)"

t5 :: String
t5 = "(lambda (a) a)"

t6 :: String
t6 = "(lambda (a) (a a))"

t7 :: String
t7 = "(let ((a) (b b)) (a a))"

t8 :: String
t8 = "(let ((a) (b b)) (a a)) (a)"

t9 :: String
t9 = "(let ((a) (b b)) (a a)) (define a) (a a)"

t10 :: String
t10 = "(let ((b b)) (a a)) (define a) (a a)"

t11 :: String
t11 = "(a b c d)"

t12 :: String
t12 = "(lambda (a b c d) e)"

t13 :: String
t13 = "(lambda () a)"

t14 :: String
t14 = "(lambda (()) a)"

t15 :: String
t15 = "(let ((a a) (b b) (c c) (d d)) e)"

t16 :: String
t16 =
  "(let ((x (add1 2))\
              \(y (sub1 x))\
              \(z (fact 5))) (+ x y z))"

errorMsg :: String
errorMsg = " parsed incorrectly"

allTests :: [Test]
allTests =
  [ TestCase $ assertEqual (testCase ++ errorMsg)
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

expErrorMsg :: String
expErrorMsg = " converted to exp incorrectly."

allExpTests :: [Test]
allExpTests =
  [ TestCase $ assertEqual
      (testCase ++ expErrorMsg)
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

failureMsg :: String
failureMsg = " did not trigger an error"

allFails :: [Test]
allFails =
  [ TestCase $ assertBool
      (f ++ failureMsg)
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
