module ParserTests
  ( parserTests
  ) where

import Errors
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
      [ (t1, return [Leaf "a"])
      , (t2, return [Node []])
      , (t3, return [Node [Leaf "a"]])
      , (t4, return [Node [Leaf "a", Leaf "a"]])
      , (t5, return [Node [Leaf "lambda", Node [Leaf "a"], Leaf "a"]])
      , ( t6
        , return
            [Node [Leaf "lambda", Node [Leaf "a"], Node [Leaf "a", Leaf "a"]]])
      , ( t7
        , return
            [ Node
                [ Leaf "let"
                , Node [Node [Leaf "a"], Node [Leaf "b", Leaf "b"]]
                , Node [Leaf "a", Leaf "a"]
                ]
            ])
      , ( t8
        , return
            [ Node
                [ Leaf "let"
                , Node [Node [Leaf "a"], Node [Leaf "b", Leaf "b"]]
                , Node [Leaf "a", Leaf "a"]
                ]
            , Node [Leaf "a"]
            ])
      , ( t9
        , return
            [ Node
                [ Leaf "let"
                , Node [Node [Leaf "a"], Node [Leaf "b", Leaf "b"]]
                , Node [Leaf "a", Leaf "a"]
                ]
            , Node [Leaf "define", Leaf "a"]
            , Node [Leaf "a", Leaf "a"]
            ])
      , ( t10
        , return
            [ Node
                [ Leaf "let"
                , Node [Node [Leaf "b", Leaf "b"]]
                , Node [Leaf "a", Leaf "a"]
                ]
            , Node [Leaf "define", Leaf "a"]
            , Node [Leaf "a", Leaf "a"]
            ])
      , (t11, return [Node [Leaf "a", Leaf "b", Leaf "c", Leaf "d"]])
      , ( t12
        , return
            [ Node
                [ Leaf "lambda"
                , Node [Leaf "a", Leaf "b", Leaf "c", Leaf "d"]
                , Leaf "e"
                ]
            ])
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
       (Right l) -> map treeToExp l)
  | (test, ex) <-
      [ (t1, [return $ Id "a"])
      , ( t2
        , [ Left $
            ParseError
              "Found (). What does this mean?\nDid you mean to make a thunk?"
          ])
      , (t3, [return $ App (Id "a") []])
      , (t4, [return $ App (Id "a") [Id "a"]])
      , (t5, [return $ Lambda ["a"] (Id "a")])
      , (t6, [return $ Lambda ["a"] (App (Id "a") [Id "a"])])
      , ( t7
        , [ return $
            App
              (Id "let")
              [ App (App (Id "a") []) [App (Id "b") [Id "b"]]
              , App (Id "a") [Id "a"]
              ]
          ])
      , ( t8
        , [ return $
            App
              (Id "let")
              [ App (App (Id "a") []) [App (Id "b") [Id "b"]]
              , App (Id "a") [Id "a"]
              ]
          , return $ App (Id "a") []
          ])
      , ( t9
        , [ return $
            App
              (Id "let")
              [ App (App (Id "a") []) [App (Id "b") [Id "b"]]
              , App (Id "a") [Id "a"]
              ]
          , return $ App (Id "define") [Id "a"]
          , return $ App (Id "a") [Id "a"]
          ])
      , ( t10
        , [ return $ Let [("b", Id "b")] (App (Id "a") [Id "a"])
          , return $ App (Id "define") [Id "a"]
          , return $ App (Id "a") [Id "a"]
          ])
      , (t11, [return $ App (Id "a") [Id "b", Id "c", Id "d"]])
      , (t12, [return $ Lambda ["a", "b", "c", "d"] $ Id "e"])
      , (t13, [return $ Lambda [] $ Id "a"])
      , ( t14
        , [ Left $
            ParseError
              "Found (). What does this mean?\nDid you mean to make a thunk?"
          ])
      , ( t15
        , [ return $
            Let
              [("a", Id "a"), ("b", Id "b"), ("c", Id "c"), ("d", Id "d")]
              (Id "e")
          ])
      ]
  ]

failureMsg = " did not trigger an error"

allFails =
  [ TestCase $
  assertBool
    (f ++ failureMsg)
    (case parse . tokenize $ f of
       (Left _) -> True
       (Right _) -> False)
  | f <- ["(", ")", "(a", "(a (a a)", "(a (a) a", "((", "(()", "(())("]
  ]

parserTests =
  TestList $
  [TestLabel ("test " ++ show i) t | (i, t) <- zip [1,2 ..] allTests] ++
  [TestLabel ("exp-test " ++ show i) t | (i, t) <- zip [1,2 ..] allExpTests] ++
  [TestLabel ("failure " ++ show i) f | (i, f) <- zip [1,2 ..] allFails]
