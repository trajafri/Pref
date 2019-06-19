module Pref
  ( eval
  , Val(..)
  ) where

import Data.Map as M
import Exp
import Lexer
import Parser
import Tokens

type Env = Map String Val

data Val
  = S String
  | I Int
  | C String
      Exp
      Env
  | T Exp --Thunk
      Env
  deriving (Eq)

instance Show Val where
  show (S s) = s
  show (I i) = show i
  show (C s b env) = "<lambda:" ++ s ++ ">"
  show (T s e) = "<thunk>"

eval :: Exp -> Env -> Maybe Val
eval (SLiteral s) = const $ Just $ S s
eval (NLiteral i) = const $ Just $ I i
eval (Id v) = M.lookup v
eval (Lambda [v] b) = Just . (C v b)
eval (Lambda (v:vs) b) = eval $ Lambda [v] $ Lambda vs b
eval (Lambda [] b) = Just . (T b)
eval (Let [(v, val)] b) =
  \env -> do
    eVal <- eval val env
    eval b (insert v eVal env)
eval (Let ((v, val):vs) b) = eval $ Let [(v, val)] $ Let vs b
eval (App rator rands) =
  (\env -> do
     eRator <- (eval rator env)
     case (eRator, rands) of
       (C v b e, []) -> Nothing
       (T b e, []) -> eval b e
       (exp, []) -> Nothing --Not a function application
       (C v b e, [rand]) ->
         eval rand env >>= (\rand -> eval b (insert v rand env))
       (C v b e, r:rs) ->
         eval r env >>= (\rand -> eval (App b rs) (insert v rand env))
       otherwise -> Nothing -- Non-closure was applied
   )

codeToVal :: String -> Maybe [Val]
codeToVal code = do
  let tokens = tokenize code
  ptree <- parse tokens
  asts <- traverse treeToExp ptree
  traverse (`eval` M.empty) asts

evaluatePref :: String -> String
evaluatePref code = maybe "Error" show $ codeToVal code
