module LexerTests
  ( lexerTests
  ) where

import Data.Char
import Lexer
import Test.HUnit
import Tokens

msg = " not tokenized correctly"

allTests =
  [ TestCase $ assertEqual (show test ++ msg) ex (tokenize test)
  | (test, ex) <-
      [ ("", [])
      , ("(", [LParen])
      , (")", [RParen])
      , ("a", [ID "a"])
      , ("(a)", [LParen, ID "a", RParen])
      , ("(lambda lambda)", [LParen, ID "lambda", ID "lambda", RParen])
      , ( "(lambda (x) (x x))"
        , [ LParen
          , ID "lambda"
          , LParen
          , ID "x"
          , RParen
          , LParen
          , ID "x"
          , ID "x"
          , RParen
          , RParen
          ])
      , ( "(define fact-5 (my-fact 5))"
        , [ LParen
          , ID "define"
          , ID "fact-5"
          , LParen
          , ID "my-fact"
          , ID "5"
          , RParen
          , RParen
          ])
      ]
  ]

lexerTests =
  TestList
    [TestLabel ("test " ++ show i) test | (i, test) <- zip [1,2 ..] allTests]
