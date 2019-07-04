module Errors
  ( Error(..)
  )
where

-- Warning: This is a really bad way to handle errors.
-- My approach is is almost as bad as the type 'AnyError'
data Error
  = ParseError String
  | EvalError String
  | LexerError String
  deriving (Show, Eq)

-- I was thinking of making an Error type class, but that seems
-- useless
