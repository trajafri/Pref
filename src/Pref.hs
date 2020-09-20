{-# LANGUAGE LambdaCase #-}

module Pref
  ( codeToVal
  , eval
  , Val(..)
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
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

eval :: Exp -> Env -> (Either Error Val)
eval e = runReaderT $ evalM e

evalM :: Exp -> ReaderT Env (Either Error) Val
evalM (SLiteral s) = return $ S s -- Strings
evalM (NLiteral i) = return $ I i -- Numbers
evalM (Id v) -- Variable
 = do
  env <- ask
  case M.lookup v env of
    Nothing -> lift . Left . EvalError $ "Can not identify " ++ v
    Just exp -> return exp
evalM (Lambda [v] b) -- Lambda base case
 = do
  env <- ask
  return $ C v b env
evalM (Lambda (v:vs) b) -- Lambda currying case
 = evalM $ Lambda [v] $ Lambda vs b
evalM (Lambda [] b) -- Thunk case
 = do
  env <- ask
  return $ T b env
evalM (Let [(v, val)] b) -- Let base case
 = do
  eValue <- evalM val
  local (insert v eValue) $ evalM b
evalM (Let ((v, val):vs) b) -- Let else case
 = evalM $ Let [(v, val)] $ Let vs b
evalM (If cond thn els) -- If case
 = do
  eCond <- evalM cond
  case eCond of
    (I 0) -> evalM els
    _ -> evalM thn
evalM (App (Id "+") rands) = evaluateNumOperation (+) 0 rands
evalM (App (Id "-") rands) = evaluateNumOperation (-) 0 rands
evalM (App (Id "*") rands) = evaluateNumOperation (*) 1 rands
evalM (App (Id "/") rands) = evaluateNumOperation div 1 rands
evalM (App (Id "string-append") rands) = evaluateStrOperation (++) "" rands
evalM (App (Id "fix") [func]) -- Z Combinator
 = evalM (Lambda ["x"] (App func [App (Id "fix") [func], Id "x"]))
evalM (App rator []) = do
  eRator <- evalM rator
  case eRator of
    (T b env) -> do
      local (const env) $ evalM b
    _ -> lift . Left . EvalError $ "Non Thunk invocation:\n" ++ show eRator
evalM (App rator [rand]) = do
  eRator <- evalM rator
  case eRator of
    (C v b env) -> do
      eRand <- evalM rand
      local (const $ insert v eRand env) $ evalM b
    _ ->
      lift . Left . EvalError $
      "Non function used as a function:\n" ++ show rator
evalM (App rator (r:rands)) = evalM (App (App rator [r]) rands)
evalM e = lift . Left . EvalError $ "Unidentified expression:\n" ++ show e

evaluateNumOperation ::
     (Int -> Int -> Int) -> Int -> [Exp] -> ReaderT Env (Either Error) Val
evaluateNumOperation op base rands = do
  maybenums <- mapM evalM rands
  nums <-
    mapM
      (\case
         (I i) -> return i
         _ ->
           lift . Left . EvalError $
           " got a non numeric argument in the following operands:\n" ++
           show rands)
      maybenums
  return . I $ Prelude.foldr op base nums

evaluateStrOperation ::
     (String -> String -> String)
  -> String
  -> [Exp]
  -> ReaderT Env (Either Error) Val
evaluateStrOperation op base rands = do
  maybestrs <- mapM evalM rands
  strs <-
    mapM
      (\case
         (S i) -> return i
         _ ->
           lift . Left . EvalError $
           " got a non string argument in the following operands:\n" ++
           show rands)
      maybestrs
  return . S $ Prelude.foldr op base strs

evalList :: [Exp] -> Env -> Either Error [Val]
evalList exps = runReaderT (evalListM exps)

evalListM :: [Exp] -> ReaderT Env (Either Error) [Val]
evalListM [] = return []
evalListM (Def id bind:es) = do
  env <- ask
  eBind <- evalM bind
  local (insert id eBind) $ evalListM es
evalListM (exp:es) = do
  eExp <- evalM exp
  eEs <- evalListM es
  return (eExp : eEs)

codeToVal :: String -> Either Error [Val]
codeToVal code = do
  tokens <- tokenize code
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
       either (print . show) (mapM_ $ print . show) (codeToVal fileContent))
