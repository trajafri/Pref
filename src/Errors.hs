module Errors
  ( Error(..)
  ) where

-- Warning: This is a really bad way to handle errors.
-- My approach is is almost as bad as the type 'AnyError'
data Error
  = ParseError String
  | EvalError String
  deriving (Show, Eq)
