{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Pref
  ( codeToAst
  , codeToVal
  , eval
  , evaluatePref
  , main
  , Val(..)
  )
where

import           Control.Monad.Reader
import qualified Control.Monad.Except          as E

import           Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Errors
import           Syntax.Exp
import           Lexer
import           Parser
import           Prelude                 hiding ( exp
                                                , id
                                                )
import           System.IO

type Env = Map T.Text Val

data Val
  = S T.Text
  | I Int
  | C T.Text
      Exp
      Env
  | T Exp --Thunk
      Env
  | Cons Val
         Val
  | E --Empty
  deriving (Eq)

instance Show Val where
  show (   S s     ) = T.unpack s
  show (   I i     ) = show i
  show (   C s _ _ ) = "<lambda:" <> T.unpack s <> ">"
  show (   T    _ _) = "<thunk>"
  show ls@(Cons _ _) = "(list"
    <> Prelude.foldr (\x y -> " " <> x <> y) ")" (contents ls)
   where
    contents (Cons a b) = show a : contents b
    contents E          = []
    contents a          = [show a]
  show E = "empty"

defaultEnv :: Env
defaultEnv = insert "empty" E M.empty

eval :: Exp -> Env -> Either Error Val
eval e env = (`runReaderT` env) . evalM $ e

evalM :: Exp -> ReaderT Env (Either Error) Val
evalM (SLiteral s) = return $ S s -- Strings
evalM (NLiteral i) = return $ I i -- Numbers
evalM (Id       v) = do
  env <- ask
  case M.lookup v env of
    Nothing  -> E.throwError . EvalError $ "Can not identify " <> v
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
evalM (App (Id "string-append") rands     ) = evaluateStrOperation (<>) "" rands
evalM (App (Id "cons"         ) [car, cdr]) = do
  eCar <- evalM car
  eCdr <- evalM cdr
  return $ Cons eCar eCdr
evalM (App (Id "car") [cons]) = do
  eCons <- evalM cons
  case eCons of
    (Cons a _) -> return a
    _          -> E.throwError . EvalError $ "Car applied to a non-list value "
evalM (App (Id "cdr") [cons]) = do
  eCons <- evalM cons
  case eCons of
    (Cons _ d) -> return d
    _          -> E.throwError . EvalError $ "Cdr applied to a non-list value"
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
      E.throwError
        .  EvalError
        $  "Non Thunk invocation:\n"
        <> (T.pack . show $ eRator)
evalM (App rator [rand]) = do
  eRator <- evalM rator
  case eRator of
    (C v b env) -> do
      eRand <- evalM rand
      local (const $ insert v eRand env) $ evalM b
    _ ->
      E.throwError
        .  EvalError
        $  "Non function used as a function:\n"
        <> (T.pack . show $ rator)
evalM (App rator (r : rands)) = evalM (App (App rator [r]) rands)
evalM e =
  E.throwError . EvalError $ "Unidentified expression:\n" <> (T.pack . show $ e)

evaluateNumOperation
  :: (Int -> Int -> Int) -> Int -> [Exp] -> ReaderT Env (Either Error) Val
evaluateNumOperation op base rands = do
  maybenums <- mapM evalM rands
  nums      <- mapM
    (\case
      (I i) -> return i
      _ ->
        E.throwError
          .  EvalError
          $  " got a non numeric argument in the following operands:\n"
          <> (T.pack . show $ rands)
    )
    maybenums
  return . I $ Prelude.foldr op base nums

evaluateStrOperation
  :: (T.Text -> T.Text -> T.Text)
  -> T.Text
  -> [Exp]
  -> ReaderT Env (Either Error) Val
evaluateStrOperation op base rands = do
  maybestrs <- mapM evalM rands
  strs      <- mapM
    (\case
      (S i) -> return i
      _ ->
        E.throwError
          .  EvalError
          $  " got a non string argument in the following operands:\n"
          <> (T.pack . show $ rands)
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

codeToAst :: T.Text -> Either Error [Exp]
codeToAst code = do
  tokens <- tokenize code
  ptree  <- parse tokens
  traverse treeToExp ptree

codeToVal :: T.Text -> Either Error [Val]
codeToVal code = do
  asts <- codeToAst code
  evalList asts defaultEnv

evaluatePref :: T.Text -> T.Text
evaluatePref code = either (T.pack . show) (T.pack . show) $ codeToVal code

main :: IO ()
main = do
  TIO.putStrLn "Enter a file path: "
  filePath <- TIO.getLine
  withFile
    (T.unpack filePath)
    ReadMode
    (\h -> do
      fileContent <- TIO.hGetContents h
      either (print . show) (mapM_ $ print . show) (codeToVal fileContent)
    )








