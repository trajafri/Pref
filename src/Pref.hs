{-# LANGUAGE LambdaCase, OverloadedStrings #-}


module Pref
  ( codeToAst
  , codeToVal
  , eval
  , evaluatePref
  , Env(..)
  , Val(..)
  , Box(..)
  )
where

import           Control.Monad.Except --For throwError
import           Control.Monad.State
import           Data.List                     as L
import           Data.Map                      as M
import qualified Data.Text                     as T
import           Errors
import           Syntax.Exp
import           Parser
import           Prelude                 hiding ( exp
                                                , id
                                                )
import           Text.Parsec             hiding ( Empty
                                                , parse
                                                )

data Env = Env {getMap :: Map T.Text Box, getCount :: Int} deriving (Eq, Show)

data Box = Thunk {getExp :: Exp}
         | Val {getVal :: Val}
  deriving (Eq, Show)

insertEnv :: T.Text -> Box -> Env -> Env
insertEnv k b (Env m i) = flip Env (succ i) $ M.insert k b m

removeEnv :: T.Text -> Env -> Env
removeEnv k (Env m i) = (flip Env i) . M.delete k $ m

updateEnv :: T.Text -> Val -> Env -> Env
updateEnv k v (Env m i) = (flip Env i) . M.adjust (const . Val $ v) k $ m

data Val
  = S T.Text
  | I Int
  | B Bool
  | C T.Text
      Exp
      Env
  | T Exp Env --Thunk
  | Cons Val
         Val
  | E --Empty
  deriving (Eq)

instance Show Val where
  show (   S s     ) = T.unpack s
  show (   I i     ) = show i
  show (   B b     ) = show b
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
defaultEnv = insertEnv "empty" (Val E) $ Env M.empty 0

eval :: Exp -> Env -> Either EvalError Val
eval e env = (`evalStateT` env) . evalM $ e

evalM :: Exp -> StateT Env (Either EvalError) Val
evalM Empty        = return E -- EmptyList 
evalM (SLiteral s) = return $ S s -- Strings
evalM (NLiteral i) = return $ I i -- Numbers
evalM (BLiteral b) = return $ B b -- Bools
evalM (Id       v) = do
  env <- get
  case M.lookup v $ getMap env of
    Nothing          -> throwError . EvalError $ "Can not identify " <> v
    Just (Thunk exp) -> undefined
    Just (Val   val) -> return val -- Variable
evalM (Lambda [v     ]   b) = gets (C v b) -- Lambda base case
evalM (Lambda (v : vs)   b) = evalM $ Lambda [v] $ Lambda vs b -- Lambda currying case
evalM (Lambda []         b) = gets (T b) -- Thunk case
evalM (Let    [(v, val)] b) = do
  vVal <- evalM val
  modify (insertEnv v (Val vVal))
  ret <- evalM b
  modify (removeEnv v)
  return ret -- Let base case
evalM (Let ((v, val) : vs) b) = evalM $ Let [(v, val)] $ Let vs b -- Let else case
evalM (If cond thn els      ) = do
  eCond <- evalM cond
  case eCond of
    (B False) -> evalM els
    _         -> evalM thn -- If case
evalM (App (Id "+"    ) rands) = evaluateNumOperation (+) 0 rands
evalM (App (Id "-"    ) rands) = evaluateNumOperation (-) 0 rands
evalM (App (Id "*"    ) rands) = evaluateNumOperation (*) 1 rands
evalM (App (Id "/"    ) rands) = evaluateNumOperation div 1 rands
evalM (App (Id "zero?") [num]) = do
  eNum <- evalM num
  return $ case eNum of
    I 0 -> B True
    _   -> B False
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
    E -> B True
    _ -> B False
evalM (App (Id "fix") [func]) = case func of
  Lambda (a : _) _ ->
    evalM $ Lambda [a] $ App func [App (Id "fix") [func], Id a] -- Z Combinator
  Lambda _ _ -> throwError . EvalError $ "fix expects a function, not a thunk"
  _          -> throwError . EvalError $ "fix expects a function"
evalM (App rator []) = do
  eRator <- evalM rator
  case eRator of
    (T b env) ->
      get
        >>= (\oEnv ->
              modify (const env)
                >>  evalM b
                >>= (\ret -> modify (const oEnv) >> return ret)
            )
    _ ->
      throwError
        .  EvalError
        $  "Non Thunk invocation:\n"
        <> (T.pack . show $ eRator)
evalM (App rator [rand]) = do
  eRator <- evalM rator
  env    <- get
  case eRator of
    (C v b localEnv) -> do
      randVal <- evalM rand
      modify (const $ insertEnv v (Val randVal) localEnv)
      ret <- evalM b
      modify (const env)
      return ret
    _ ->
      throwError
        .  EvalError
        $  "Non function used as a function:\n"
        <> (T.pack . show $ rator)
evalM (App rator (r : rands)) = evalM (App (App rator [r]) rands)
evalM e =
  throwError . EvalError $ "Unidentified expression:\n" <> (T.pack . show $ e)

evaluateNumOperation
  :: (Int -> Int -> Int) -> Int -> [Exp] -> StateT Env (Either EvalError) Val
evaluateNumOperation op base rands = do
  maybenums <- mapM evalM rands
  nums      <- mapM
    (\case
      (I i) -> return i
      e ->
        throwError
          .  EvalError
          $  " got a non numeric argument in the following operand:\n"
          <> (T.pack . show $ e)
    )
    maybenums
  return . I $ Prelude.foldr op base nums

evaluateStrOperation
  :: (T.Text -> T.Text -> T.Text)
  -> T.Text
  -> [Exp]
  -> StateT Env (Either EvalError) Val
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

evalList :: [Exp] -> [(T.Text, Exp)] -> Env -> Either EvalError [Val]
evalList []                    _              _   = return []
evalList (Def id binding : es) futureBindings env = do
  let newFutures   = L.drop 1 futureBindings
  let fixedBinding = topLevelFunction id newFutures binding
  val <- eval fixedBinding env
  evalList es newFutures $ insertEnv id (Val val) env
 where
  topLevelFunction :: T.Text -> [(T.Text, Exp)] -> Exp -> Exp
  topLevelFunction expId fb (Lambda ps body) = App
    (Id "fix")
    [Lambda (expId : ps) $ L.foldr (Let . return) body $ futureFunctions fb]
  topLevelFunction _ _ b = b

  futureFunctions :: [(T.Text, Exp)] -> [(T.Text, Exp)]
  futureFunctions fs = (`evalState` fs) $ forM fs $ \(name, func) -> do
    modify $ L.drop 1
    currFutureBindings <- get
    return (name, topLevelFunction name currFutureBindings func)
evalList (exp : es) fb env = do
  eExp  <- eval exp env
  eExps <- evalList es fb env
  return $ eExp : eExps

codeToAst :: T.Text -> Either ParseError [Exp]
codeToAst code = either throwError return $ runParser parse () "" code

codeToVal :: T.Text -> Either EvalError (Either ParseError [Val])
codeToVal code = case codeToAst code of
  Left  e   -> return . Left $ e
  Right ast -> case evalList ast (futureBindings ast) defaultEnv of
    Left  e    -> Left e
    Right vals -> return . Right $ vals
  where futureBindings ast = [ (i, b) | (Def i b) <- ast ]

evaluatePref :: T.Text -> T.Text
evaluatePref =
  either (T.pack . show) (either (T.pack . show) (T.pack . show)) . codeToVal
