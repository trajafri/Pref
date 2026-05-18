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
    PrefComputation (PrefE),
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List.NonEmpty as NE (singleton)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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
    -- Note that a closure doesn't contain an `Exp`
    C Identifier PrefComputation Scope
  | -- | Thunk
    T [Exp] Scope
  | Cons Val Val
  | -- | Empty list
    E
  | -- | Void
    V
  deriving (Eq)

instance Show Val where
  show (S s) = T.unpack s
  show (I i) = show i
  show (B b) = show b
  show (C (Var s) _ _) = "<lambda:" <> T.unpack s <> ">"
  show (T _ _) = "<thunk>"
  show ls@(Cons _ _) =
    "(list"
      <> foldr (\x y -> " " <> x <> y) ")" (contents ls)
    where
      contents (Cons a b) = show a : contents b
      contents E = []
      contents a = [show a]
  show E = "empty"
  show V = "<void>"

-- Interpreter can see two kinds of computations
-- 1. An expression written in Pref.
-- 2. A program written in Haskell.
data PrefComputation = PrefE Exp | HaskE (EStack Val)

-- `Eq` is implemented only for testing purposes
instance Eq PrefComputation where
  (PrefE exp1) == (PrefE exp2) = exp1 == exp2
  -- Since there's no point to testing two partial Haskell
  -- computations, all cases with `HaskE` are not equal
  _ == _ = False

newtype MemAddress d = Addr Int deriving (Eq, Show, Ord)

type Scope = MemAddress Env

newtype Env = Env
  { getMap :: M.Map Identifier ComputationPtr
  }
  deriving (Eq, Show)

type ComputationPtr = MemAddress Computation

-- For a strict interpreter, we never use the `Computation` constructor
data Computation
  = Computed Val
  | -- | a value that hasn't been computed yet,
    Computation Exp Scope
  deriving (Eq, Show)

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

insertEnv :: Identifier -> ComputationPtr -> Env -> Env
insertEnv k b = Env . M.insert k b . getMap

getVal :: Identifier -> Env -> Maybe ComputationPtr
getVal var = M.lookup var . getMap

getEnvForCurrentScope :: EStack Env
getEnvForCurrentScope = do
  scope <- ask
  scopeDeref scope

resolveIdentifierToPtr :: Identifier -> Env -> EStack ComputationPtr
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

computationAlloc :: Exp -> Scope -> EStack ComputationPtr
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

computationWrite :: ComputationPtr -> Computation -> EStack ()
computationWrite ptr cmp = do
  St memC memE <- get
  put $ St (mset ptr cmp memC) memE

computationDeref :: ComputationPtr -> EStack Computation
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
        $ "<Internal Memory error when fetching environment>"

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
evalM (Begin _) = throwError . EvalError $ "Begin: under construction"
evalM (Lambda [] [body]) = asks $ T [body] -- Thunk case
evalM (Lambda [identifier] [body]) =
  asks $ C identifier (PrefE body) -- Lambda base case
evalM (Lambda (identifier : ids) [body]) = evalM $ Lambda [identifier] [Lambda ids [body]] -- Lambda currying case
evalM (Lambda _ _) = throwError . EvalError $ "Lambda Begin: under construction"
evalM (Let bindings [body]) = evalInLocalScope bindings $ evalM body
evalM (Let _ _) = throwError . EvalError $ "Let Begin: under construction"
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
evalM (Def _ _) =
  throwError
    . EvalError
    $ "A non-top level `defined` expression is not supported"

-- TODO: Memory can be updated by top-level expressions in cbr
evalList :: [Exp] -> EStack [Val]
evalList expList = do
  scope <- ask
  St memC memE <- get
  let env0 = fromMaybe (Env M.empty) $ mget scope memE
  either throwError return (helper expList futureBindings scope env0 memC memE)
  where
    futureBindings = [(i, b) | (Def i b) <- expList]
    helper [] _ _ _ _ _ = return []
    helper (Def id binding : es) fBs scp env memCmp memEnv =
      let newFutures = drop 1 fBs
          fixedBinding = topLevelFunction id newFutures binding
          (uMemC, memId) = malloc (Computation fixedBinding scp) memCmp
          val = memId
          newEnv = insertEnv id val env
          uMemE = mset scp newEnv memEnv
       in helper es newFutures scp newEnv uMemC uMemE
      where
        topLevelFunction :: Identifier -> [(Identifier, Exp)] -> Exp -> Exp
        topLevelFunction expId fb (Lambda [] body) =
          App
            (App (Id . Var $ "fix") [Lambda [expId, self] [Lambda [] [newBody]]])
            [NLiteral 0]
          where
            self = Var "_" -- Todo: Should be a non-colliding variable
            newBody =
              foldr
                (\b r -> Let (NE.singleton b) [r])
                ( Let
                    (NE.singleton (expId, App (Id expId) [Id self]))
                    body
                )
                fns
            fns = futureFunctions fb <> [(expId, App (Id expId) [Id self])]
        topLevelFunction expId fb (Lambda ps [body]) =
          App
            (Id . Var $ "fix")
            [Lambda (expId : ps) [foldr (\b r -> Let (NE.singleton b) [r]) body $ futureFunctions fb]]
        topLevelFunction _ _ (Lambda _ _) = error "Under construction"
        topLevelFunction _ _ b = b

        futureFunctions :: [(Identifier, Exp)] -> [(Identifier, Exp)]
        futureFunctions fs = (`evalState` fs) $ forM fs $ \(name, func) -> do
          modify $ drop 1
          currFutureBindings <- get
          return (name, topLevelFunction name currFutureBindings func)
    helper (exp : es) fb scp env memCmp memEnv =
      (:) <$> eval exp scp memCmp memEnv <*> helper es fb scp env memCmp memEnv

-- Utilities
--------------------------------------------------------------------

resolveIdentifier :: Identifier -> EStack Val
resolveIdentifier identifier = do
  env <- getEnvForCurrentScope
  cmpPtr <- resolveIdentifierToPtr identifier env
  runComputationAndSave cmpPtr

runComputationAndSave :: ComputationPtr -> EStack Val
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
  localEnv <- foldM (\newEnv (id, exp) -> addBinding id exp newEnv) env bindings
  localScope <- scopeAlloc localEnv
  local (const localScope) localEval

addBinding :: Identifier -> Exp -> Env -> EStack Env
addBinding id exp env = do
  scp <- ask
  ptr <- computationAlloc exp scp
  return $ insertEnv id ptr env

invokeThunk :: Val -> EStack Val
invokeThunk (T body capturedScope) =
  evalInCapturedScope capturedScope Nothing $ evalM . head $ body
invokeThunk v =
  throwError
    . EvalError
    $ "Bad application\nExpected thunk, got " <> (T.pack . show $ v)

applyClosure :: Val -> Exp -> [Exp] -> EStack Val
applyClosure (C identifier body scope) rand0 randRest =
  let bodyEval :: EStack Val
      bodyEval = evalInCapturedScope scope [(identifier, rand0)] $
        case body of
          (PrefE exp) -> evalM exp
          (HaskE comp) -> comp
   in case randRest of
        [] -> bodyEval
        (rand1 : rands) -> do
          closure <- bodyEval
          applyClosure closure rand1 rands
applyClosure _ _ _ =
  throwError
    . EvalError
    $ "Bad application\nA non-function was used like a function"
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
          <> [(func, createUnary code) | (func, code) <- unaries]
          <> [(func, createBinary code) | (func, code) <- binaries]

      (defaultEnv, memC) =
        foldr
          ( \(builtin, val) (e, m) ->
              let (newMem, ptr) = malloc (Computed val) m
               in (insertEnv (Var builtin) ptr e, newMem)
          )
          (Env M.empty, Mem M.empty 0)
          defaultBindings
      memE = fst $ malloc defaultEnv (Mem M.empty scopeAdr)
   in (globalScope, memE, memC)
  where
    scopeAdr = 0
    globalScope = Addr scopeAdr

    createBuiltIn :: Int -> EStack Val -> Val
    createBuiltIn m comp = compute m globalScope
      where
        compute 1 = C (Var "1") (HaskE comp)
        compute n =
          C identifier (HaskE . asks . compute . pred $ n)
          where
            identifier = Var . T.pack . show $ n

    createBinary binOp = createBuiltIn 2 $ do
      v1 <- resolveIdentifier $ Var "2"
      v2 <- resolveIdentifier $ Var "1"
      binOp v1 v2

    createUnary unOp = createBuiltIn 1 $ do
      v1 <- resolveIdentifier $ Var "1"
      unOp v1

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
    safeFix (C identifier (PrefE b) scope) = do
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
    Right vals -> return . Right $ vals
  where
    (scope, memE, defaultMem) = prepareDefaultBindings

evaluatePref :: T.Text -> T.Text
evaluatePref =
  either (T.pack . show) (either (T.pack . show) (T.pack . show)) . codeToVal
