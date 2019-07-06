{-# LANGUAGE OverloadedStrings #-}

module CpserTests
  ( cpserTestList
  )
where

import           Data.Either
import qualified Data.Text                     as T
import           Syntax.Exp
import           Pref
import           Test.HUnit
import           Transform.CPS

errorMsg :: T.Text
errorMsg = " cpsed incorrectly"

getAst :: T.Text -> Exp
getAst = head . fromRight [Id "error"] . codeToAst

allTests :: [Test]
allTests =
  [ TestCase $ assertEqual (show $ testCase <> errorMsg)
                           (getAst e)
                           (cpser . getAst $ testCase)
  | (testCase, e) <-
    [ ("1", "1")
    , ("\"S\"", "\"S\"")
    , ("x", "x")
    , ("(lambda (x) x)", "(lambda (x k) (k x))")
    , ("(lambda (x) (a b))", "(lambda (x k) (a b (lambda (arg0) (k arg0))))")
    , ( "(lambda (x) (a (b c)))"
      , "(lambda (x k) (b c (lambda (arg0) (a arg0 (lambda (arg1) (k arg1))))))"
      )
    , ( "(lambda (x)\
             \(func (a b) (c (d (e f)))))"
      , "(lambda (x k)\
             \(a b (lambda (arg0)\
                     \(e f (lambda (arg1)\
                             \(d arg1 (lambda (arg2)\
                                        \(c arg2 (lambda (arg3)\
                                                   \(func arg0 arg3 (lambda (arg4)\
                                                                      \(k arg4))))))))))))"
      )
-- Example below is bin-to-decimal
    , ( "(lambda (n) (if (empty? n) 0 (+ (car n) (* 2 (bin-to-decimal (cdr n))))))"
      , "(lambda (n k)\
             \(empty? n\
                     \(lambda (arg0)\
                       \(if arg0\
                           \(k 0)\
                           \(car n (lambda (arg0)\
                                    \(cdr n (lambda (arg1)\
                                             \(bin-to-decimal arg1 (lambda (arg2)\
                                                                    \(* 2 arg2 (lambda (arg3)\
                                                                                 \(+ arg0 arg3 (lambda (arg4) (k arg4)))))))))))))))"
      )
-- Ackermann
    , ( "(lambda (m n)\
            \(if (zero? m) (add1 n)\
                \(if (zero? n) (ack (sub1 m) 1)\
                    \(ack (sub1 m) (ack m (sub1 n))))))"
      , "(lambda (m n k)\
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
      )
    , ( "(let ((x (add1 2))\
              \(y (sub1 3))\
              \(z (fact 5))) (+ x y z))"
      , "(add1 2 (lambda (arg0)\
                  \(sub1 3 (lambda (arg1)\
                            \(fact 5 (lambda (arg2)\
                                      \((lambda (x y z k)\
                                         \(+ x \
                                            \y \
                                            \z\
                                            \(lambda (arg0)\
                                              \(k arg0)))) arg0 \
                                                           \arg1 \
                                                           \arg2\
                                                          \(lambda (arg3) arg3))))))))"
      )
    , ( "(let ((x (add1 2))\
              \(y 2)\
              \(z (sub1 5))) (+ x 2 z))"
      , "(add1 2 (lambda (arg0)\
                  \(sub1 5 (lambda (arg1)\
                            \((lambda (x y z k)\
                                 \(+ x 2 z (lambda (arg0)\
                                              \(k arg0)))) arg0 \
                                                           \2 \
                                                           \arg1\
                                                          \(lambda (arg2) arg2))))))"
      )
    ]
  ]

cpserTestList :: Test
cpserTestList = TestList
  [ TestLabel ("test " ++ show i) t
  | (i, t) <- zip ([1, 2 ..] :: [Int]) allTests
  ]
