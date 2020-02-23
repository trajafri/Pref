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
[prefQuoter|
(define powerset
  (lambda (ls)
            (if (empty? ls)
                (cons empty empty)
                (let ((recurse (powerset (cdr ls))))
                   (++ (map (cons (car ls)) recurse) recurse)))))
|]
