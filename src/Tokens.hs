module Tokens
  ( Token(..)
  )
where

data Token
  = LParen
  | RParen
  | ID String
  deriving (Eq)

instance Show Token where
  show LParen = "("
  show RParen = ")"
  show (ID v) = v
