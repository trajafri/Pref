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

import           Control.Monad.Except --For throwError
import           Control.Monad.Reader
import           Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Errors
import           Syntax.Exp
import           Parser
import           Prelude                 hiding ( exp
                                                , id
                                                )
import           System.IO
import           Text.Parsec             hiding ( parse )

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

eval :: Exp -> Env -> Either EvalError Val
eval e env = (`runReaderT` env) . evalM $ e

evalM :: Exp -> ReaderT Env (Either EvalError) Val
evalM (SLiteral s) = return $ S s -- Strings
evalM (NLiteral i) = return $ I i -- Numbers
evalM (Id       v) = do
  env <- ask
  case M.lookup v env of
    Nothing  -> throwError . EvalError $ "Can not identify " <> v
    Just exp -> return exp -- Variable
evalM (Lambda [v     ]   b) = asks (C v b) -- Lambda base case
evalM (Lambda (v : vs)   b) = evalM $ Lambda [v] $ Lambda vs b -- Lambda currying case
evalM (Lambda []         b) = asks (T b) -- Thunk case
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
    _          -> throwError . EvalError $ "Car applied to a non-list value "
evalM (App (Id "cdr") [cons]) = do
  eCons <- evalM cons
  case eCons of
    (Cons _ d) -> return d
    _          -> throwError . EvalError $ "Cdr applied to a non-list value"
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
      throwError
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
      throwError
        .  EvalError
        $  "Non function used as a function:\n"
        <> (T.pack . show $ rator)
evalM (App rator (r : rands)) = evalM (App (App rator [r]) rands)
evalM e =
  throwError . EvalError $ "Unidentified expression:\n" <> (T.pack . show $ e)

evaluateNumOperation
  :: (Int -> Int -> Int) -> Int -> [Exp] -> ReaderT Env (Either EvalError) Val
evaluateNumOperation op base rands = do
  maybenums <- mapM evalM rands
  nums      <- mapM
    (\case
      (I i) -> return i
      _ ->
        throwError
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
  -> ReaderT Env (Either EvalError) Val
evaluateStrOperation op base rands = do
  maybestrs <- mapM evalM rands
  strs      <- mapM
    (\case
      (S i) -> return i
      _ ->
        throwError
          .  EvalError
          $  " got a non string argument in the following operands:\n"
          <> (T.pack . show $ rands)
    )
    maybestrs
  return . S $ Prelude.foldr op base strs

evalList :: [Exp] -> Env -> Either EvalError [Val]
evalList []                 _   = return []
evalList (Def id bind : es) env = do
  eBind <- eval bind env
  evalList es (insert id eBind env)
evalList (exp : es) env = do
  eExp  <- eval exp env
  eExps <- evalList es env
  return $ eExp : eExps

codeToAst :: T.Text -> Either ParseError [Exp]
codeToAst code =
  either throwError return $ runParser parse () "" . T.unpack $ code

codeToVal :: T.Text -> Either EvalError (Either ParseError [Val])
codeToVal code = case codeToAst code of
  (Left  e  ) -> return . Left $ e
  (Right ast) -> case evalList ast defaultEnv of
    (Left  e   ) -> Left e
    (Right vals) -> return . Right $ vals

evaluatePref :: T.Text -> T.Text
evaluatePref =
  either (T.pack . show) (either (T.pack . show) (T.pack . show)) . codeToVal

main :: IO ()
main = do
  TIO.putStrLn "Enter a file path: "
  filePath <- TIO.getLine
  withFile
    (T.unpack filePath)
    ReadMode
    (\h -> do
      fileContent <- TIO.hGetContents h
      either (print . show) (either (print . show) $ mapM_ $ print . show)
        $ codeToVal fileContent
    )
