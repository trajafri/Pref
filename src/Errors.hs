module Errors
  ( Error(..)
  )
where

import qualified Data.Text                     as T

-- Warning: This is a really bad way to handle errors.
-- My approach is is almost as bad as the type 'AnyError'
data Error
  = ParseError T.Text
  | EvalError T.Text
  | LexerError T.Text
  deriving (Show, Eq)

-- I was thinking of making an Error type class, but that seems
-- useless
