module Syntax.Tokens
  ( Token(..)
  )
where

import qualified Data.Text                     as T

data Token
  = LParen
  | RParen
  | ID T.Text
  deriving (Eq)

instance Show Token where
  show LParen = "("
  show RParen = ")"
  show (ID v) = T.unpack v
