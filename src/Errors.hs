module Errors
  ( EvalError(..)
  )
where

import qualified Data.Text                     as T

data EvalError = EvalError T.Text deriving (Show, Eq)
