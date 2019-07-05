module Syntax.Exp
  ( Exp(..)
  )
where

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
  show (Id       v ) = v
  show (NLiteral v ) = show v
  show (SLiteral v ) = v
  show (Lambda vs b) = "(lambda (" ++ unwords vs ++ ") " ++ show b ++ ")"
  show (If c thn els) =
    "(if " ++ show c ++ " " ++ show thn ++ " " ++ show els ++ ")"
  show (Let bindings body) =
    "(let (("
      ++ unwords (map (\(v, b) -> v ++ show b) bindings)
      ++ ")) "
      ++ show body
      ++ ")"
  show (App tor nds) = "(app " ++ show tor ++ " " ++ show nds ++ ")"
  show (Def v   b  ) = "(define " ++ v ++ " " ++ show b
