module Pref
  ( codeToVal
  , eval
  , Val(..)
  ) where

import Data.Map as M
import Errors
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

eval :: Exp -> Env -> Either Error Val
eval (SLiteral s) = const $ return $ S s -- Strings. Not implemented yet
eval (NLiteral i) = const $ return $ I i -- Numbers
eval (Id v) =
  maybe (Left $ EvalError $ "Can not identify " ++ v) Right . M.lookup v -- Variable
eval (Lambda [v] b) = return . (C v b) -- Lambda base case
eval (Lambda (v:vs) b) = eval $ Lambda [v] $ Lambda vs b -- Lambda currying case
eval (Lambda [] b) = return . (T b) -- Thunk case
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
eval (App (Id "string-append") rands) = evaluateStrOperation (++) "" rands
eval (App rator rands) =
  \env -> do
    eRator <- (eval rator env)
    case (eRator, rands) of
      (C v b e, []) ->
        Left $ EvalError "Function expected arguments applied to nothing"
      (T b e, []) -> eval b e
      (C v b e, [rand]) ->
        eval rand env >>= (\rand -> eval b (insert v rand env))
      (C v b e, r:rs) ->
        eval r env >>= (\rand -> eval (App b rs) (insert v rand env))
      otherwise ->
        Left $ EvalError $ "Non function used as a function:\n" ++ show rator --Not a function application
eval e = const $ Left $ EvalError $ "Unidentified expression:\n" ++ show e

evaluateNumOperation ::
     (Int -> Int -> Int) -> Int -> [Exp] -> Env -> Either Error Val
evaluateNumOperation op base rands env = do
  maybenums <- mapM (`eval` env) rands
  nums <-
    mapM
      (\x ->
         case x of
           (I i) -> return i
           _ ->
             Left $
             EvalError $
             " got a non numeric argument in the following operands:\n" ++
             show rands)
      maybenums
  return $ I $ Prelude.foldr op base nums

evaluateStrOperation ::
     (String -> String -> String) -> String -> [Exp] -> Env -> Either Error Val
evaluateStrOperation op base rands env = do
  maybestrs <- mapM (`eval` env) rands
  strs <-
    mapM
      (\x ->
         case x of
           (S i) -> return i
           _ ->
             Left $
             EvalError $
             " got a non string argument in the following operands:\n" ++
             show rands)
      maybestrs
  return $ S $ Prelude.foldr op base strs

evalList :: [Exp] -> Env -> Either Error [Val]
evalList [] _ = return []
evalList (Def id bind:es) e = do
  eBind <- eval bind e
  evalList es (insert id eBind e)
evalList (exp:es) e = do
  eExp <- eval exp e
  eEs <- evalList es e
  return (eExp : eEs)

codeToVal :: String -> Either Error [Val]
codeToVal code = do
  let tokens = tokenize code
  ptree <- parse tokens
  asts <- traverse treeToExp ptree
  evalList asts M.empty

evaluatePref :: String -> String
evaluatePref code = either show show $ codeToVal code

main = do
  putStrLn "Enter a file path: "
  filePath <- getLine
  withFile
    filePath
    ReadMode
    (\h -> do
       fileContent <- hGetContents h
       either
         (putStrLn . show)
         (mapM_ $ putStrLn . show)
         (codeToVal fileContent))
