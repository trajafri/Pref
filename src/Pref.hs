{-# LANGUAGE LambdaCase #-}

module Pref
  ( codeToAst
  , codeToVal
  , eval
  , evaluatePref
  , main
  , Val(..)
  )
where

import           Control.Monad.Trans

import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Reader
import           Data.Map                      as M
import           Errors
import           Exp
import           Lexer
import           Parser
import           Prelude                 hiding ( exp
                                                , id
                                                )
import           System.IO

type Env = Map String Val

data Val
  = S String
  | I Int
  | C String
      Exp
      Env
  | T Exp --Thunk
      Env
  | Cons Val
         Val
  | E --Empty
  deriving (Eq)

instance Show Val where
  show (   S s     ) = s
  show (   I i     ) = show i
  show (   C s _ _ ) = "<lambda:" ++ s ++ ">"
  show (   T    _ _) = "<thunk>"
  show ls@(Cons _ _) = "(list"
    ++ Prelude.foldr (\x y -> " " ++ x ++ y) ")" (contents ls)
   where
    contents (Cons a b) = show a : contents b
    contents E          = []
    contents a          = [show a]
  show E = "empty"

defaultEnv :: Env
defaultEnv = insert "empty" E M.empty

liftEither :: Either a b -> ReaderT Env (ContT r (Either a)) b
liftEither = lift . lift

eval :: Exp -> Env -> Either Error Val
eval e env = (`runContT` Right) . (`runReaderT` env) . evalM $ e

evalM :: Exp -> ReaderT Env (ContT Val (Either Error)) Val
evalM (SLiteral s) = return $ S s -- Strings
evalM (NLiteral i) = return $ I i -- Numbers
evalM (Id       v) = do
  env <- ask
  case M.lookup v env of
    Nothing  -> liftEither . Left . EvalError $ "Can not identify " ++ v
    Just exp -> return exp -- Variable
evalM (Lambda [v     ]   b) = C v b <$> ask -- Lambda base case
evalM (Lambda (v : vs)   b) = evalM $ Lambda [v] $ Lambda vs b -- Lambda currying case
evalM (Lambda []         b) = T b <$> ask -- Thunk case
evalM (Let    [(v, val)] b) = do
  eValue <- evalM val
  local (insert v eValue) $ evalM b -- Let base case
evalM (Let ((v, val) : vs) b) = evalM $ Let [(v, val)] $ Let vs b -- Let else case
evalM (If cond thn els      ) = do
  eCond <- evalM cond
  case eCond of
    (I 0) -> evalM els
    _     -> evalM thn -- If case
evalM (App (Id "+"            ) rands     ) = evaluateNumOperation (+) 0 rands
evalM (App (Id "-"            ) rands     ) = evaluateNumOperation (-) 0 rands
evalM (App (Id "*"            ) rands     ) = evaluateNumOperation (*) 1 rands
evalM (App (Id "/"            ) rands     ) = evaluateNumOperation div 1 rands
evalM (App (Id "string-append") rands     ) = evaluateStrOperation (++) "" rands
evalM (App (Id "cons"         ) [car, cdr]) = do
  eCar <- evalM car
  eCdr <- evalM cdr
  return $ Cons eCar eCdr
evalM (App (Id "car") [cons]) = do
  eCons <- evalM cons
  case eCons of
    (Cons a _) -> return a
    _ -> liftEither . Left . EvalError $ "Car applied to a non-list value "
evalM (App (Id "cdr") [cons]) = do
  eCons <- evalM cons
  case eCons of
    (Cons _ d) -> return d
    _ -> liftEither . Left . EvalError $ "Cdr applied to a non-list value"
evalM (App (Id "empty?") [ls]) = do
  eLs <- evalM ls
  return $ case eLs of
    E -> I 1
    _ -> I 0
evalM (App (Id "fix") [func]) =
  evalM (Lambda ["x"] (App func [App (Id "fix") [func], Id "x"])) -- Z Combinator
evalM (App rator []) = do
  eRator <- evalM rator
  case eRator of
    (T b env) -> local (const env) $ evalM b
    _ ->
      liftEither . Left . EvalError $ "Non Thunk invocation:\n" ++ show eRator
evalM (App rator [rand]) = do
  eRator <- evalM rator
  case eRator of
    (C v b env) -> do
      eRand <- evalM rand
      local (const $ insert v eRand env) $ evalM b
    _ ->
      liftEither
        .  Left
        .  EvalError
        $  "Non function used as a function:\n"
        ++ show rator
evalM (App rator (r : rands)) = evalM (App (App rator [r]) rands)
evalM e =
  liftEither . Left . EvalError $ "Unidentified expression:\n" ++ show e

evaluateNumOperation
  :: (Int -> Int -> Int)
  -> Int
  -> [Exp]
  -> ReaderT Env (ContT Val (Either Error)) Val
evaluateNumOperation op base rands = do
  maybenums <- mapM evalM rands
  nums      <- mapM
    (\case
      (I i) -> return i
      _ ->
        liftEither
          .  Left
          .  EvalError
          $  " got a non numeric argument in the following operands:\n"
          ++ show rands
    )
    maybenums
  return . I $ Prelude.foldr op base nums

evaluateStrOperation
  :: (String -> String -> String)
  -> String
  -> [Exp]
  -> ReaderT Env (ContT Val (Either Error)) Val
evaluateStrOperation op base rands = do
  maybestrs <- mapM evalM rands
  strs      <- mapM
    (\case
      (S i) -> return i
      _ ->
        liftEither
          .  Left
          .  EvalError
          $  " got a non string argument in the following operands:\n"
          ++ show rands
    )
    maybestrs
  return . S $ Prelude.foldr op base strs

evalList :: [Exp] -> Env -> Either Error [Val]
evalList []                 _   = return []
evalList (Def id bind : es) env = do
  eBind <- eval bind env
  evalList es (insert id eBind env)
evalList (exp : es) env = do
  eExp  <- eval exp env
  eExps <- evalList es env
  return $ eExp : eExps

codeToAst :: String -> Either Error [Exp]
codeToAst code = do
  tokens <- tokenize code
  ptree  <- parse tokens
  traverse treeToExp ptree

codeToVal :: String -> Either Error [Val]
codeToVal code = do
  asts <- codeToAst code
  evalList asts defaultEnv

evaluatePref :: String -> String
evaluatePref code = either show show $ codeToVal code

main :: IO ()
main = do
  putStrLn "Enter a file path: "
  filePath <- getLine
  withFile
    filePath
    ReadMode
    (\h -> do
      fileContent <- hGetContents h
      either (print . show) (mapM_ $ print . show) (codeToVal fileContent)
    )
