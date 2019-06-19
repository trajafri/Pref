module ParserTests
  ( parserTests
  ) where

import Exp
import Lexer
import Parser
import Test.HUnit

-- These tests depend on Lexer.Tokenize
t1 = "a"

t2 = "()"

t3 = "(a)"

t4 = "(a a)"

t5 = "(lambda (a) a)"

t6 = "(lambda (a) (a a))"

t7 = "(let ((a) (b b)) (a a))"

t8 = "(let ((a) (b b)) (a a)) (a)"

t9 = "(let ((a) (b b)) (a a)) (define a) (a a)"

t10 = "(let ((b b)) (a a)) (define a) (a a)"

t11 = "(a b c d)"

t12 = "(lambda (a b c d) e)"

t13 = "(lambda () a)"

t14 = "(lambda (()) a)"

t15 = "(let ((a a) (b b) (c c) (d d)) e)"

errorMsg = " parsed incorrectly"

allTests =
  [ TestCase $ assertEqual (test ++ errorMsg) ex (parse . tokenize $ test)
  | (test, ex) <-
      [ (t1, Just [Leaf "a"])
      , (t2, Just [Node []])
      , (t3, Just [Node [Leaf "a"]])
      , (t4, Just [Node [Leaf "a", Leaf "a"]])
      , (t5, Just [Node [Leaf "lambda", Node [Leaf "a"], Leaf "a"]])
      , ( t6
        , Just
            [Node [Leaf "lambda", Node [Leaf "a"], Node [Leaf "a", Leaf "a"]]])
      , ( t7
        , Just
            [ Node
                [ Leaf "let"
                , Node [Node [Leaf "a"], Node [Leaf "b", Leaf "b"]]
                , Node [Leaf "a", Leaf "a"]
                ]
            ])
      , ( t8
        , Just
            [ Node
                [ Leaf "let"
                , Node [Node [Leaf "a"], Node [Leaf "b", Leaf "b"]]
                , Node [Leaf "a", Leaf "a"]
                ]
            , Node [Leaf "a"]
            ])
      , ( t9
        , Just
            [ Node
                [ Leaf "let"
                , Node [Node [Leaf "a"], Node [Leaf "b", Leaf "b"]]
                , Node [Leaf "a", Leaf "a"]
                ]
            , Node [Leaf "define", Leaf "a"]
            , Node [Leaf "a", Leaf "a"]
            ])
      , ( t10
        , Just
            [ Node
                [ Leaf "let"
                , Node [Node [Leaf "b", Leaf "b"]]
                , Node [Leaf "a", Leaf "a"]
                ]
            , Node [Leaf "define", Leaf "a"]
            , Node [Leaf "a", Leaf "a"]
            ])
      , (t11, Just [Node [Leaf "a", Leaf "b", Leaf "c", Leaf "d"]])
      , ( t12
        , Just
            [ Node
                [ Leaf "lambda"
                , Node [Leaf "a", Leaf "b", Leaf "c", Leaf "d"]
                , Leaf "e"
                ]
            ])
      , (t13, Just [Node [Leaf "lambda", Node [], Leaf "a"]])
      , (t14, Just [Node [Leaf "lambda", Node [Node []], Leaf "a"]])
      , ( t15
        , Just
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
            ])
      ]
  ]

expErrorMsg = " converted to exp incorrectly."

allExpTests =
  [ TestCase $
  assertEqual
    (test ++ expErrorMsg)
    ex
    (case parse . tokenize $ test of
       (Just l) -> map treeToExp l)
  | (test, ex) <-
      [ (t1, [Just $ Id "a"])
      , (t2, [Nothing])
      , (t3, [Just $ App (Id "a") []])
      , (t4, [Just $ App (Id "a") [Id "a"]])
      , (t5, [Just $ Lambda ["a"] (Id "a")])
      , (t6, [Just $ Lambda ["a"] (App (Id "a") [Id "a"])])
      , ( t7
        , [ Just $
            App
              (Id "let")
              [ App (App (Id "a") []) [App (Id "b") [Id "b"]]
              , App (Id "a") [Id "a"]
              ]
          ])
      , ( t8
        , [ Just $
            App
              (Id "let")
              [ App (App (Id "a") []) [App (Id "b") [Id "b"]]
              , App (Id "a") [Id "a"]
              ]
          , Just $ App (Id "a") []
          ])
      , ( t9
        , [ Just $
            App
              (Id "let")
              [ App (App (Id "a") []) [App (Id "b") [Id "b"]]
              , App (Id "a") [Id "a"]
              ]
          , Just $ App (Id "define") [Id "a"]
          , Just $ App (Id "a") [Id "a"]
          ])
      , ( t10
        , [ Just $ Let [("b", Id "b")] (App (Id "a") [Id "a"])
          , Just $ App (Id "define") [Id "a"]
          , Just $ App (Id "a") [Id "a"]
          ])
      , (t11, [Just $ App (Id "a") [Id "b", Id "c", Id "d"]])
      , (t12, [Just $ Lambda ["a", "b", "c", "d"] $ Id "e"])
      , (t13, [Just $ Lambda [] $ Id "a"])
      , (t14, [Nothing])
      , ( t15
        , [ Just $
            Let
              [("a", Id "a"), ("b", Id "b"), ("c", Id "c"), ("d", Id "d")]
              (Id "e")
          ])
      ]
  ]

failureMsg = " did not trigger an error"

allFails =
  [ TestCase $ assertEqual (f ++ failureMsg) Nothing (parse . tokenize $ f)
  | f <- ["(", ")", "(a", "(a (a a)", "(a (a) a", "((", "(()", "(())("]
  ]

parserTests =
  TestList $
  [TestLabel ("test " ++ show i) t | (i, t) <- zip [1,2 ..] allTests] ++
  [TestLabel ("exp-test " ++ show i) t | (i, t) <- zip [1,2 ..] allExpTests] ++
  [TestLabel ("failure " ++ show i) f | (i, f) <- zip [1,2 ..] allFails]
