module LexerTests
  ( lexerTests
  ) where

import Data.Char
import Errors
import Lexer
import Test.HUnit
import Tokens

msg = " not tokenized correctly"

allTests =
  [ TestCase $ assertEqual (show test ++ msg) (Right ex) (tokenize test)
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
      , ("\"hellothisisastring!\"", [ID "\"hellothisisastring!\""])
      , ("\"hello this is a string!\"", [ID "\"hello this is a string!\""])
      , ( "id(define\"string\"2id2"
        , [ID "id", LParen, ID "define", ID "\"string\"", ID "2", ID "id2"])
        --Yay, white sapces work now!
      ]
  ]

failureMsg = "Lexer did not trigger an error in the following case:\n"

lexerError =
  Left . LexerError $ "LexerError: A string was not terminated with a quote."

errorTests =
  [ TestCase $ assertEqual (failureMsg ++ test) lexerError $ tokenize test
  | test <- ["\"this is an error"]
  ]

lexerTests =
  TestList $
  [TestLabel ("test " ++ show i) test | (i, test) <- zip [1,2 ..] allTests] ++
  [ TestLabel ("error test " ++ show i) test
  | (i, test) <- zip [1,2 ..] errorTests
  ]
