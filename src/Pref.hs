{-# LANGUAGE OverloadedStrings #-}

module Pref
  ( codeToAst,
    codeToVal,
    prepareDefaultBindings,
    eval,
    evaluatePref,
    Env (..),
    Val (..),
    Computation (..),
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import Errors
import Parser
import Syntax.Exp
import Text.Parsec (ParseError, runParser)
import Prelude hiding (exp, id)

-- Type Setup
-----------------------------------------------------------------

data Val
  = S T.Text
  | I Int
  | B Bool
  | -- | Closure
    C Identifier Exp Scope
  | -- | Thunk
    T Exp Scope
  | -- | Internal code
    Internal T.Text (ValPtr -> EStack Val)
  | Cons Val Val
  | -- | Empty list
    E
  | -- | Void
    V

instance Show Val where
  show (S s) = T.unpack s
  show (I i) = show i
  show (B b) = show b
  show (C (Var s) _ _) = "<lambda:" <> T.unpack s <> ">"
  show (T _ _) = "<thunk>"
  show (Internal n _) = "<internal " <> T.unpack n <> ">"
  show ls@(Cons _ _) =
    "(list"
      <> foldr (\x y -> " " <> x <> y) ")" (contents ls)
    where
      contents (Cons a b) = show a : contents b
      contents E = []
      contents a = [show a]
  show E = "empty"
  show V = "<void>"

newtype MemAddress d = Addr Int deriving (Eq, Show, Ord)

type Scope = MemAddress Env

newtype Env = Env
  { getMap :: M.Map Identifier ValPtr
  }
  deriving (Eq, Show)

type ValPtr = MemAddress Computation

-- For a strict interpreter, we never use the `Computation` constructor
data Computation
  = Computed Val
  | -- | a value that hasn't been computed yet,
    Computation Exp Scope
  deriving (Show)

data Mem d = Mem
  { getMappings :: M.Map (MemAddress d) d,
    getCounter :: Int
  }
  deriving (Show)

data EvalState
  = St
  { getComputationMappings :: Mem Computation,
    getEnvironmentMappings :: Mem Env
  }
  deriving (Show)

-- interpreter's monad stack
type EStack val =
  StateT EvalState (ReaderT Scope (Either EvalError)) val

-- env utilities
-----------------------------------------------------------------

insertEnv :: Identifier -> ValPtr -> Env -> Env
insertEnv k b = Env . M.insert k b . getMap

getVal :: Identifier -> Env -> Maybe ValPtr
getVal var = M.lookup var . getMap

getEnvForCurrentScope :: EStack Env
getEnvForCurrentScope = do
  scope <- ask
  scopeDeref scope

resolveIdentifierToPtr :: Identifier -> Env -> EStack ValPtr
resolveIdentifierToPtr identifier@(Var identifierTxt) env = do
  case getVal identifier env of
    Nothing ->
      throwError
        . EvalError
        $ "Can not identify variable '"
          <> identifierTxt
          <> "'"
    Just cmpPtr -> return cmpPtr

-- state/memory utilities
-----------------------------------------------------------------

initMem :: Mem a
initMem = Mem M.empty 0

malloc :: a -> Mem a -> (Mem a, MemAddress a)
malloc v m =
  let i = getCounter m
      address = Addr i
      nextAddress = succ i
      newMap = M.insert address v . getMappings $ m
   in (Mem newMap nextAddress, address)

mset :: MemAddress a -> a -> Mem a -> Mem a
mset id v (Mem m i) = Mem newMap i where newMap = M.adjust (const v) id m

mget :: MemAddress a -> Mem a -> Maybe a
mget id = M.lookup id . getMappings

computationAlloc :: Exp -> Scope -> EStack ValPtr
computationAlloc exp scope = do
  St memC memE <- get
  let (newMemC, ptr) = malloc (Computation exp scope) memC
  put $ St newMemC memE
  return ptr

scopeAlloc :: Env -> EStack Scope
scopeAlloc env = do
  St memC memE <- get
  let (newMemE, scope) = malloc env memE
  put $ St memC newMemE
  return scope

computationWrite :: ValPtr -> Computation -> EStack ()
computationWrite ptr cmp = do
  St memC memE <- get
  put $ St (mset ptr cmp memC) memE

scopeWrite :: Scope -> Env -> EStack ()
scopeWrite scope env = do
  St memC memE <- get
  put $ St memC (mset scope env memE)

computationDeref :: ValPtr -> EStack Computation
computationDeref ptr = do
  memC <- gets getComputationMappings
  case mget ptr memC of
    Just cmp -> return cmp
    Nothing ->
      throwError . EvalError $ "<Internal Memory error when fetching computation>"

scopeDeref :: Scope -> EStack Env
scopeDeref scope = do
  memE <- gets getEnvironmentMappings
  case mget scope memE of
    Just env -> return env
    Nothing ->
      throwError
        . EvalError
        $ "<Internal Memory error when fetching environment " <> (T.pack . show $ scope) <> ">"

-- Interpreter
--------------------------------------------------------------------

runEval :: Scope -> Mem Computation -> Mem Env -> EStack val -> Either EvalError val
runEval scope memC memE = (`runReaderT` scope) . (`evalStateT` St memC memE)

eval :: Exp -> Scope -> Mem Computation -> Mem Env -> Either EvalError Val
eval e envscope memC memE = runEval envscope memC memE . evalM $ e

evalM :: Exp -> EStack Val
evalM (SLiteral s) = return $ S s -- Strings
evalM (NLiteral i) = return $ I i -- Numbers
evalM (BLiteral b) = return $ B b -- Bools
evalM (Id identifier) = resolveIdentifier identifier
evalM (Begin es) = snd <$> evalList es
evalM (Lambda [] body) =
  -- Thunk case
  asks $ T . Begin $ body
evalM (Lambda [identifier] body) =
  -- Lambda base case
  asks $ C identifier (Begin body)
evalM (Lambda (identifier : ids) body) =
  -- Lambda currying case
  evalM $ Lambda [identifier] [Lambda ids body]
evalM (Let bindings body) =
  evalInLocalScope bindings $ evalM (Begin body)
evalM (If cond thn els) = do
  -- If case
  eCond <- evalM cond
  case eCond of
    (B False) -> evalM els
    _ -> evalM thn
evalM (App rator []) = do
  -- Thunk application
  ratorVal <- evalM rator
  invokeThunk ratorVal
evalM (App rator (rand : rands)) = do
  -- Function application
  ratorVal <- evalM rator
  applyClosure ratorVal rand rands
evalM (Def i e) = do
  addBinding i e
  return V

evalList :: [Exp] -> EStack ([Val], Val)
evalList [] = return ([], V)
evalList [e] = evalM e >>= \v -> return ([v], v)
evalList (e : es) = do
  v <- evalM e
  (res, lastVal) <- evalList es
  case v of
    V -> return (res, lastVal)
    _ -> return (v : res, lastVal)

-- Utilities
--------------------------------------------------------------------

resolveIdentifier :: Identifier -> EStack Val
resolveIdentifier identifier = do
  env <- getEnvForCurrentScope
  cmpPtr <- resolveIdentifierToPtr identifier env
  runComputationAndSave cmpPtr

runComputationAndSave :: ValPtr -> EStack Val
runComputationAndSave ptr = do
  comp <- computationDeref ptr
  val <- runComputation comp
  computationWrite ptr (Computed val)
  return val

runComputation :: Computation -> EStack Val
runComputation cmp = case cmp of
  Computed value -> return value
  Computation exp cmpScpe -> local (const cmpScpe) $ evalM exp

evalInLocalScope :: (Foldable t) => t (Identifier, Exp) -> EStack Val -> EStack Val
evalInLocalScope bindings localEval = do
  scope <- ask
  evalInCapturedScope scope bindings localEval

evalInCapturedScope :: (Foldable t) => Scope -> t (Identifier, Exp) -> EStack Val -> EStack Val
evalInCapturedScope scope bindings localEval = do
  env <- scopeDeref scope
  localEnv <-
    foldM
      (\newEnv (id, exp) -> addBindingToEnv id exp newEnv)
      env
      bindings
  localScope <- scopeAlloc localEnv
  local (const localScope) localEval

addBinding :: Identifier -> Exp -> EStack ()
addBinding id exp = do
  scope <- ask
  env <- getEnvForCurrentScope
  newEnv <- addBindingToEnv id exp env
  scopeWrite scope newEnv

addBindingToEnv :: Identifier -> Exp -> Env -> EStack Env
addBindingToEnv id exp env = do
  ptr <- expToValPtr exp
  return $ insertEnv id ptr env

expToValPtr :: Exp -> EStack ValPtr
expToValPtr exp = do
  scp <- ask
  computationAlloc exp scp

invokeThunk :: Val -> EStack Val
invokeThunk (T body capturedScope) =
  evalInCapturedScope capturedScope Nothing $ evalM body
invokeThunk v =
  throwError
    . EvalError
    $ "Bad application\nExpected thunk, got " <> (T.pack . show $ v)

applyClosure :: Val -> Exp -> [Exp] -> EStack Val
applyClosure v rand0 randRest =
  case randRest of
    [] -> bodyEval
    (rand1 : rands) -> do
      closure <- bodyEval
      applyClosure closure rand1 rands
  where
    bodyEval :: EStack Val
    bodyEval = case v of
      (C identifier body scope) ->
        evalInCapturedScope scope [(identifier, rand0)] $ evalM body
      (Internal _ f) -> do
        ptr <- expToValPtr rand0
        f ptr
      _ ->
        throwError . EvalError $
          "Bad application\nA non-function was used like a function"
            <> "\nPerhaps a function was applied to too many arguments?"

-- Setup for usage
-----------------------------------------------------------------

prepareDefaultBindings :: (Scope, Mem Env, Mem Computation)
prepareDefaultBindings =
  let constants :: [(T.Text, Val)]
      constants = [("empty", E)]

      unaries :: [(T.Text, Val -> EStack Val)]
      unaries =
        [ ("car", safeCar),
          ("cdr", safeCdr),
          ("zero?", zeroHuh),
          ("empty?", emptyHuh),
          ("fix", safeFix),
          ("not", safeNot)
        ]

      binaries :: [(T.Text, Val -> Val -> EStack Val)]
      binaries =
        [ ("+", safePlus),
          ("-", safeMinus),
          ("*", safeMult),
          ("/", safeDiv),
          ("string-append", safeAppend),
          ("cons", safeCons),
          ("and", safeAnd),
          ("or", safeOr)
        ]

      defaultBindings :: [(T.Text, Val)]
      defaultBindings =
        constants
          <> [(func, createUnary func code) | (func, code) <- unaries]
          <> [(func, createBinary func code) | (func, code) <- binaries]

      (defaultEnv, memC) =
        foldr
          ( \(builtin, val) (e, m) ->
              let (newMem, ptr) = malloc (Computed val) m
               in (insertEnv (Var builtin) ptr e, newMem)
          )
          (Env M.empty, initMem)
          defaultBindings
      initMemE = initMem
      memE = fst $ malloc defaultEnv initMemE
   in (Addr . getCounter $ initMemE, memE, memC)
  where
    createBinary :: T.Text -> (Val -> Val -> EStack Val) -> Val
    createBinary n binOp = Internal n $
      \vPtr -> do
        v <- runComputationAndSave vPtr
        return . createUnary n $ binOp v

    createUnary :: T.Text -> (Val -> EStack Val) -> Val
    createUnary n unOp = Internal n $ runComputationAndSave >=> unOp

    safePlus :: Val -> Val -> EStack Val
    safePlus (I n) (I m) = return . I $ n + m
    safePlus _ _ = throwError . EvalError $ "Expected two numbers"

    safeMinus :: Val -> Val -> EStack Val
    safeMinus (I n) (I m) = return . I $ n - m
    safeMinus _ _ = throwError . EvalError $ "Expected two numbers"

    safeMult :: Val -> Val -> EStack Val
    safeMult (I n) (I m) = return . I $ n * m
    safeMult a b = throwError . EvalError $ "Expected two numbers, got " <> (T.pack . show $ a) <> " and " <> (T.pack . show $ b)

    safeDiv :: Val -> Val -> EStack Val
    safeDiv (I n) (I m) = return . I $ n `div` m
    safeDiv _ _ = throwError . EvalError $ "Expected two numbers"

    safeAppend :: Val -> Val -> EStack Val
    safeAppend (S a) (S b) = return . S $ a <> b
    safeAppend _ _ = throwError . EvalError $ "Expected two strings"

    safeCons :: Val -> Val -> EStack Val
    safeCons a b = return $ Cons a b

    safeAnd :: Val -> Val -> EStack Val
    safeAnd a@(B False) _ = return a
    safeAnd _ b = return b

    safeOr :: Val -> Val -> EStack Val
    safeOr (B False) b = return b
    safeOr a _ = return a

    safeCar :: Val -> EStack Val
    safeCar (Cons a _) = return a
    safeCar _ = throwError . EvalError $ "Expected a list"

    safeCdr :: Val -> EStack Val
    safeCdr (Cons _ b) = return b
    safeCdr _ = throwError . EvalError $ "Expected a list"

    zeroHuh :: Val -> EStack Val
    zeroHuh (I 0) = return $ B True
    zeroHuh _ = return $ B False

    emptyHuh :: Val -> EStack Val
    emptyHuh E = return $ B True
    emptyHuh _ = return $ B False

    -- TODO: Consider fix for thunks
    safeFix :: Val -> EStack Val
    safeFix (C identifier b scope) = do
      evalInCapturedScope scope [(identifier, b)] $ do
        localScope <- ask
        localEnv <- getEnvForCurrentScope
        bPtr <- resolveIdentifierToPtr identifier localEnv
        computationWrite bPtr (Computation b localScope)
        resolveIdentifier identifier
    safeFix _ = throwError . EvalError $ "fix expects a non-zero arity function"

    safeNot :: Val -> EStack Val
    safeNot (B False) = return $ B True
    safeNot _ = return $ B False

codeToAst :: T.Text -> Either ParseError [Exp]
codeToAst code = either throwError return $ runParser parse () "" code

codeToVal :: T.Text -> Either ParseError (Either EvalError [Val])
codeToVal code = case codeToAst code of
  Left e -> Left e
  Right ast -> case runEval scope defaultMem memE $ evalList ast of
    Left e -> return . Left $ e
    Right (vals, _) -> return . Right $ vals
  where
    (scope, memE, defaultMem) = prepareDefaultBindings

evaluatePref :: T.Text -> T.Text
evaluatePref =
  either (T.pack . show) (either (T.pack . show) (T.pack . show)) . codeToVal
