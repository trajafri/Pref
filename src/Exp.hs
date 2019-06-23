module Exp
  ( Exp(..)
  ) where

data Exp
  = Id String
  | NLiteral Int
  | SLiteral String
  | Lambda [String]
           Exp
  | If Exp
       Exp
       Exp
  | Let [(String, Exp)]
        Exp
  | App Exp
        [Exp]
  | Def String
        Exp
  deriving (Eq)

instance Show Exp where
  show (Id v) = "Id " ++ v
  show (NLiteral v) = show v
  show (SLiteral v) = v
  show (Lambda vs b) = "(Lambda (" ++ unwords vs ++ ") " ++ show b ++ ")"
  show (If c thn els) =
    "(If " ++ show c ++ " " ++ show thn ++ " " ++ show els ++ ")"
  show (Let bindings b) =
    "(Let ((" ++
    unwords (map (\(v, b) -> v ++ (show b)) bindings) ++ ")) " ++ show b ++ ")"
  show (App tor nds) = "(App " ++ show tor ++ " " ++ show nds ++ ")"
