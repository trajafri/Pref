module Pref
  ( eval
  , Val(..)
  ) where

import Data.Map as M
import Exp
import Lexer
import Parser
import System.IO
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
eval (SLiteral s) = const $ Just $ S s -- Strings. Not implemented yet
eval (NLiteral i) = const $ Just $ I i -- Numbers
eval (Id v) = M.lookup v -- Variable
eval (Lambda [v] b) = Just . (C v b) -- Lambda base case
eval (Lambda (v:vs) b) = eval $ Lambda [v] $ Lambda vs b -- Lambda currying case
eval (Lambda [] b) = Just . (T b) -- Thunk case
eval (Let [(v, val)] b) =
  \env -> do
    eVal <- eval val env
    eval b (insert v eVal env)
eval (Let ((v, val):vs) b) = eval $ Let [(v, val)] $ Let vs b
eval (If cond thn els) =
  \env -> do
    eCond <- eval cond env
    case eCond of
      (I 0) -> eval els env
      _ -> eval thn env
eval (App (Id "+") rands) = evaluateNumOperation (+) 0 rands
eval (App (Id "-") rands) = evaluateNumOperation (-) 0 rands
eval (App (Id "*") rands) = evaluateNumOperation (*) 1 rands
eval (App (Id "/") rands) = evaluateNumOperation (div) 1 rands
eval (App rator rands) =
  \env -> do
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
eval _ = const Nothing

evaluateNumOperation :: (Int -> Int -> Int) -> Int -> [Exp] -> Env -> Maybe Val
evaluateNumOperation op base rands env = do
  maybenums <- mapM (`eval` env) rands
  nums <-
    mapM
      (\x ->
         case x of
           (I i) -> Just i
           _ -> Nothing)
      maybenums
  return $ I $ Prelude.foldr op base nums

evalList :: [Exp] -> Env -> Maybe [Val]
evalList [] _ = Just []
evalList (Def id bind:es) e = do
  eBind <- eval bind e
  evalList es (insert id eBind e)
evalList (exp:es) e = do
  eExp <- eval exp e
  eEs <- evalList es e
  return (eExp : eEs)

codeToVal :: String -> Maybe [Val]
codeToVal code = do
  let tokens = tokenize code
  ptree <- parse tokens
  asts <- traverse treeToExp ptree
  evalList asts M.empty

evaluatePref :: String -> String
evaluatePref code = maybe "Error" show $ codeToVal code

main = do
  putStrLn "Enter a file path: "
  filePath <- getLine
  withFile
    filePath
    ReadMode
    (\h -> do
       fileContent <- hGetContents h
       putStrLn "Read the following"
       putStrLn fileContent
       maybe
         (putStrLn "Error" >> return ())
         (mapM_ $ putStrLn . show)
         (codeToVal fileContent))
