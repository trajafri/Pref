{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Example.THExample where

import           TH

fact :: Int -> Int
[prefQuoter|
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))|]

powerset :: [Int] -> [[Int]]
powerset = [prefQuoter|
           (lambda (ls)
                     (if (empty? ls)
                         (cons empty empty)
                         (let ((recurse (powerset (cdr ls))))
                            (++ (map (cons (car ls)) recurse) recurse))))
|]

fib :: Int -> Int
fib = [prefQuoter|
       (lambda (n) (haskellFib n))|]
 where
  haskellFib 0 = 0
  haskellFib 1 = 1
  haskellFib n = haskellFib (pred n) + haskellFib (pred $ pred n)
