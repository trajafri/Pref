module Exp
  ( Exp(..)
  ) where

data Exp
  = Id String
  | NLiteral Int
  | SLiteral String
  | Lambda String
           Exp
  | Let (String, Exp)
        Exp
  | App Exp
        Exp
  deriving (Eq)

instance Show Exp where
  show (Id v) = "Id " ++ v
  show (Lambda v b) = "(Lambda (" ++ v ++ ") " ++ show b ++ ")"
  show (Let (v, bi) b) =
    "(Let ((" ++ v ++ " " ++ show bi ++ ")) " ++ show b ++ ")"
  show (App tor nd) = "(App " ++ show tor ++ " " ++ show nd ++ ")"
