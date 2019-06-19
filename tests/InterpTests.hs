module InterpTests
  ( interpTestList
  ) where

import Data.Map as M
import Exp
import Lexer
import Parser
import Pref
import Test.HUnit

errorMsg = " interpreted incorrectly"

allTests =
  [ TestCase $
  assertEqual
    (show test ++ errorMsg)
    (Just e)
    (maybe Nothing (`eval` empty) $
     maybe Nothing treeToExp $ (head <$>) . parse . tokenize $ test)
  | (e, test) <-
      [ (I 1, "1")
      --, (S "a", "\"a\"")
      , (C "x" (NLiteral 2) empty, "(lambda (x) 2)")
      , (I 2, "((lambda (x) 2) 3)")
      , (I 2, "((lambda (x) 2) 3)")
      ]
  ]

interpTestList =
  TestList [TestLabel ("test " ++ show i) t | (i, t) <- zip [1,2 ..] allTests]
