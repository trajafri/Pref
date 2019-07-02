module CpserTests
  ( cpserTestList
  ) where

import Data.Either
import Exp
import Pref
import Test.HUnit
import Transform.CPS

-- TODO: Use parser here instead of making the ast by hand.
errorMsg :: String
errorMsg = " cpsed incorrectly"

getAst :: String -> Exp
getAst = head . fromRight [] . codeToAst

allTests :: [Test]
allTests =
  [ TestCase $
  assertEqual (show testCase ++ errorMsg) (getAst e) (cpser . getAst $ testCase)
  | (e, testCase) <-
      [ ("1", "1")
      , ("\"S\"", "\"S\"")
      , ("x", "x")
      , ("(lambda (x k) (k x))", "(lambda (x) x)")
      , ("(lambda (x k) (a b (lambda (arg0) (k arg0))))", "(lambda (x) (a b))")
      , ( "(lambda (x k) (b c (lambda (arg0) (a arg0 (lambda (arg1) (k arg1))))))"
        , "(lambda (x) (a (b c)))")
      , ( "(lambda (x k)\
             \(a b (lambda (arg0)\
                     \(e f (lambda (arg1)\
                             \(d arg1 (lambda (arg2)\
                                        \(c arg2 (lambda (arg3)\
                                                   \(func arg0 arg3 (lambda (arg4)\
                                                                      \(k arg4))))))))))))"
        , "(lambda (x)\
             \(func (a b) (c (d (e f)))))")
        -- Example below is bin-to-decimal from C311's cpsing assignment
      , ( "(lambda (n k)\
             \(empty? n\
                     \(lambda (arg0)\
                       \(if arg0\
                           \(k 0)\
                           \(car n (lambda (arg0)\
                                    \(cdr n (lambda (arg1)\
                                             \(bin-to-decimal arg1 (lambda (arg2)\
                                                                    \(* 2 arg2 (lambda (arg3)\
                                                                                 \(+ arg0 arg3 (lambda (arg4) (k arg4)))))))))))))))"
        , "(lambda (n) (if (empty? n) 0 (+ (car n) (* 2 (bin-to-decimal (cdr n))))))")
        -- Ackermann
      , ( "(lambda (m n k)\
            \(zero? m (lambda (arg0)\
                       \(if arg0 (add1 n (lambda (arg0) (k arg0)))\
                                \(zero? n\
                                  \(lambda (arg0)\
                                    \(if arg0\
                                        \(sub1 m (lambda (arg0)\
                                                  \(ack arg0 1 (lambda (arg1) (k arg1)))))\
                                         \(sub1 m (lambda (arg0)\
                                                  \(sub1 n (lambda (arg1)\
                                                    \(ack m arg1 (lambda (arg2)\
                                                                  \(ack arg0 arg2\
                                                                       \(lambda (arg3) (k arg3)\
                                                                       \)))))))))))))))"
        , "(lambda (m n)\
            \(if (zero? m) (add1 n)\
                \(if (zero? n) (ack (sub1 m) 1)\
                    \(ack (sub1 m) (ack m (sub1 n))))))")
      ]
  ]

cpserTestList :: Test
cpserTestList =
  TestList
    [ TestLabel ("test " ++ show i) t
    | (i, t) <- zip ([1,2 ..] :: [Int]) allTests
    ]
