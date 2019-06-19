module Pref
  ( eval
  , Val(..)
  ) where

import Data.Map as M
import Exp

type Env = Map String Val

data Val
  = S String
  | I Int
  | C String
      Exp
      Env
  deriving (Show, Eq)

eval :: Exp -> Env -> Maybe Val
eval (SLiteral s) = const $ Just $ S s
eval (NLiteral i) = const $ Just $ I i
eval (Id v) = M.lookup v
eval (Lambda v b) = Just . (C v b)
eval (App rator rand) =
  (\env -> do
     eRator <- (eval rator env)
     eRand <- (eval rand env)
     case eRator of
       C v b e -> eval b (insert v eRand e)
       otherwise -> Nothing -- Non-closure was applied
   )
