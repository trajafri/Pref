module Errors
  ( EvalError(..)
  )
where

import qualified Data.Text                     as T

newtype EvalError = EvalError T.Text deriving (Show, Eq)
