{-# LANGUAGE LambdaCase, OverloadedStrings #-}


module Pref
  ( codeToAst
  , codeToVal
  , defaultEnv
  , eval
  , evaluatePref
  , Env(..)
  , Val(..)
  , Box(..)
  )
where

import           Control.Monad.Except --For throwError
import           Control.Monad.State
import           Control.Monad.Reader
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

data Env = Env {getMap :: Map T.Text Val} deriving (Eq, Show)

data Mem a = Mem (Map Int a) Int

data Box = Thunk Exp Env
         | Val Val
  deriving (Eq, Show)

insertEnv :: T.Text -> Val -> Env -> Env
insertEnv k b = Env . M.insert k b . getMap

insertMem :: a -> Mem a -> (Mem a, Int)
insertMem v (Mem m i) = (Mem (M.insert i v m) $ succ i, i)

updateMem :: Int -> a -> Mem a -> Mem a
updateMem id v (Mem m i) = (flip Mem i) . M.adjust (const v) id $ m

getMemMapping :: Int -> Mem a -> Maybe a
getMemMapping id (Mem m _) = M.lookup id m

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
  | Bx Int -- Memory reference
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
  show E      = "empty"
  show (Bx _) = "<box>"

defaultEnv :: Env
defaultEnv = insertEnv "empty" (Bx 0) . Env $ M.empty

defaultMem :: Mem Box
defaultMem = fst . insertMem (Val E) . Mem M.empty $ 0

eval :: Exp -> Env -> Mem Box -> Either EvalError Val
eval e env mem = (`runReaderT` env) . (`evalStateT` mem) . evalM $ e

evalM :: Exp -> StateT (Mem Box) (ReaderT Env (Either EvalError)) Val
evalM Empty          = return E -- EmptyList 
evalM (SLiteral s  ) = return $ S s -- Strings
evalM (NLiteral i  ) = return $ I i -- Numbers
evalM (BLiteral b  ) = return $ B b -- Bools
evalM (Id       var) = do
  env <- ask
  case M.lookup var $ getMap env of
    Nothing      -> throwError . EvalError $ "Can not identify " <> var
    Just (Bx id) -> do
      b <- gets $ getMemMapping id
      case b of
        Just (Thunk exp oldEnv) -> do
          val <- local (const oldEnv) $ evalM exp
          modify $ updateMem id (Val val)
          return val
        Just (Val v) -> return v
        _            -> throwError . EvalError $ "Memory error " <> var
    Just v -> throwError . EvalError $ "Laziness error " <> (T.pack . show $ v)
evalM (Lambda [v     ]   b) = asks (C v b) -- Lambda base case
evalM (Lambda (v : vs)   b) = evalM $ Lambda [v] $ Lambda vs b -- Lambda currying case
evalM (Lambda []         b) = asks (T b) -- Thunk case
evalM (Let    [(v, exp)] b) = do
  mem <- get
  env <- ask
  let (uMem, id) = insertMem (Thunk exp env) mem
  let vVal       = Bx id
  put uMem
  local (insertEnv v vVal) $ evalM b -- Let base case
evalM (Let ((v, exp) : vs) b) = evalM $ Let [(v, exp)] $ Let vs b -- Let else case
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
      currEnv <- ask
      mem     <- get
      case rand of
        Id var -> do
          case M.lookup var $ getMap currEnv of
            Nothing      -> throwError . EvalError $ "Can not identify " <> var
            Just (Bx id) -> local (const $ insertEnv v (Bx id) env) $ evalM b
            Just _       -> throwError . EvalError $ "Laziness error " <> var
        _ -> do
          let (uMem, id) = insertMem (Thunk rand currEnv) mem
          let randVal    = Bx id
          put uMem
          local (const $ insertEnv v randVal env) $ evalM b
    _ ->
      throwError
        .  EvalError
        $  "Non function used as a function:\n"
        <> (T.pack . show $ rator)
evalM (App rator (r : rands)) = evalM (App (App rator [r]) rands)
evalM e =
  throwError . EvalError $ "Unidentified expression:\n" <> (T.pack . show $ e)

evaluateNumOperation
  :: (Int -> Int -> Int)
  -> Int
  -> [Exp]
  -> StateT (Mem Box) (ReaderT Env (Either EvalError)) Val
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
  -> StateT (Mem Box) (ReaderT Env (Either EvalError)) Val
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

--TODO: Memory can be updated by top-level expressions in cbr
evalList :: [Exp] -> [(T.Text, Exp)] -> Env -> Mem Box -> Either EvalError [Val]
evalList [] _ _ _ = return []
evalList (Def id binding : es) futureBindings env mem =
  let newFutures   = L.drop 1 futureBindings
      fixedBinding = topLevelFunction id newFutures binding
      (uMem, mid)  = insertMem (Thunk fixedBinding env) mem
      val          = Bx mid
  in  evalList es newFutures (insertEnv id val env) uMem
 where
  topLevelFunction :: T.Text -> [(T.Text, Exp)] -> Exp -> Exp
  topLevelFunction expId fb (Lambda [] body) =
    App
        (App
          (Id "fix")
          [ Lambda [expId, "_"] -- Todo: Should be a non-colliding variable
            . Lambda []
            $ L.foldr (Let . return)
                      (Let [(expId, App (Id expId) [Id "_"])] body)
            $ futureFunctions fb
          ]
        )
      $ [NLiteral 0]
  topLevelFunction expId fb (Lambda ps body) = App
    (Id "fix")
    [Lambda (expId : ps) $ L.foldr (Let . return) body $ futureFunctions fb]
  topLevelFunction _ _ b = b

  futureFunctions :: [(T.Text, Exp)] -> [(T.Text, Exp)]
  futureFunctions fs = (`evalState` fs) $ forM fs $ \(name, func) -> do
    modify $ L.drop 1
    currFutureBindings <- get
    return (name, topLevelFunction name currFutureBindings func)
evalList (exp : es) fb env mem =
  (:) <$> eval exp env mem <*> evalList es fb env mem

codeToAst :: T.Text -> Either ParseError [Exp]
codeToAst code = either throwError return $ runParser parse () "" code

codeToVal :: T.Text -> Either EvalError (Either ParseError [Val])
codeToVal code = case codeToAst code of
  Left  e   -> return . Left $ e
  Right ast -> case evalList ast (futureBindings ast) defaultEnv defaultMem of
    Left  e    -> Left e
    Right vals -> return . Right $ vals
  where futureBindings ast = [ (i, b) | (Def i b) <- ast ]

evaluatePref :: T.Text -> T.Text
evaluatePref =
  either (T.pack . show) (either (T.pack . show) (T.pack . show)) . codeToVal
