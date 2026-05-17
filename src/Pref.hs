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

-- For throwError

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (first)
import Data.List.NonEmpty as NE (singleton)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Errors
import Parser
import Syntax.Exp
import Text.Parsec hiding
  ( Empty,
    parse,
  )
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

-- interpreter's monad stack
type EStack val =
  StateT (Mem Computation, Mem Env) (ReaderT Scope (Either EvalError)) val

-- Interpreter
--------------------------------------------------------------------

runEval :: Scope -> Mem Computation -> Mem Env -> EStack val -> Either EvalError val
runEval scope memC memE = (`runReaderT` scope) . (`evalStateT` (memC, memE))

eval :: Exp -> Scope -> Mem Computation -> Mem Env -> Either EvalError Val
eval e envscope memC memE = runEval envscope memC memE . evalM $ e

evalM :: Exp -> EStack Val
evalM (SLiteral s) = return $ S s -- Strings
evalM (NLiteral i) = return $ I i -- Numbers
evalM (BLiteral b) = return $ B b -- Bools
evalM (Id identifier) = getMemoizedValue identifier
evalM (Begin _) = throwError . EvalError $ "Begin: under construction"
evalM (Lambda [] [body]) = asks $ T [body] -- Thunk case
evalM (Lambda [identifier] [body]) =
  asks $ C identifier (PrefE body) -- Lambda base case
evalM (Lambda (identifier : ids) [body]) = evalM $ Lambda [identifier] [Lambda ids [body]] -- Lambda currying case
evalM (Lambda _ _) = throwError . EvalError $ "Lambda Begin: under construction"
evalM (Let bindings [body]) = do
  -- Let case
  env <- getEnvForCurrentScope
  updatedEnv <- foldM pushToEnv env bindings
  newScope <- getNewScope updatedEnv
  local (const newScope) . evalM $ body
  where
    pushToEnv :: Env -> (Identifier, Exp) -> EStack Env
    pushToEnv newEnv (identifier, exp) = do
      memAdd <- memoize exp
      return $ insertEnv identifier memAdd newEnv
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
  case ratorVal of
    (T body env) -> local (const env) . evalM . head $ body
    _ ->
      throwError
        . EvalError
        $ "Bad application\nA thunk was applied to arguments"
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
  (memC, memE) <- get
  let env0 = fromMaybe (Env M.empty) $ getMemMapping scope memE
  either throwError return (helper expList futureBindings scope env0 memC memE)
  where
    futureBindings = [(i, b) | (Def i b) <- expList]
    helper [] _ _ _ _ _ = return []
    helper (Def id binding : es) fBs scp env memCmp memEnv =
      let newFutures = drop 1 fBs
          fixedBinding = topLevelFunction id newFutures binding
          (uMemC, memId) = insertMem (Computation fixedBinding scp) memCmp
          val = memId
          newEnv = insertEnv id val env
          uMemE = updateMem scp newEnv memEnv
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

applyClosure :: Val -> Exp -> [Exp] -> EStack Val
applyClosure (C identifier body scope) rand remainingRands = do
  memAddress <- memoize rand
  env <- getEnvForScope scope
  let localEnv = insertEnv identifier memAddress env
  localScope <- getNewScope localEnv
  -- Note: the variable below just tells us how to run the computation
  --       to evaluate the closure's body. It doesn't actually run it
  let evalBody = local (const localScope) $ case body of
        (PrefE exp) -> evalM exp
        (HaskE comp) -> comp
  case remainingRands of
    [] -> evalBody -- no more arguments, so just return whatever body returns
    (r : rs) -> do
      -- body better return a closure. Apply it to rest of rands
      clos <- evalBody
      applyClosure clos r rs
applyClosure _ _ _ =
  throwError
    . EvalError
    $ "Bad application\nA non-function was used like a function"
      <> "\nPerhaps a function was applied to too many arguments?"

getMemoizedValue :: Identifier -> EStack Val
getMemoizedValue identifier@(Var identifierTxt) = do
  memAddress <- resolveIdentifier identifier
  cmp <- gets $ getMemMapping memAddress . fst
  case cmp of
    Just (Computation exp cmpScpe) -> do
      -- memoize computed value
      val <- local (const cmpScpe) $ evalM exp
      modify . first $ updateMem memAddress (Computed val)
      return val
    Just (Computed value) -> return value
    Nothing -> throwError . EvalError $ "<Internal Memory error>" <> identifierTxt

-- If given a variable, get's the memory address for the value it points to
-- Else, places the exp in the memory table and returns its memory address
-- This should be used whenever a *variable is bound to a value* to stay lazy
memoize :: Exp -> EStack ComputationPtr
memoize (Id var) =
  -- A bound variable, therefore it's a computation we have seen before.
  -- be careful and make sure the computation isn't evaluated (to stay lazy)
  resolveIdentifier var
memoize exp = do
  currScope <- ask
  (mC, mE) <- get
  let (updatedMemTable, memAddress) = insertMem (Computation exp currScope) mC
  put (updatedMemTable, mE)
  return memAddress

resolveIdentifier :: Identifier -> EStack ComputationPtr
resolveIdentifier identifier@(Var identifierTxt) = do
  env <- getEnvForCurrentScope
  case getVal identifier env of
    Just v -> return v
    Nothing ->
      throwError
        . EvalError
        $ "Can not identify variable '"
          <> identifierTxt
          <> "'"

getEnvForScope :: Scope -> EStack Env
getEnvForScope scope = do
  (_, memE) <- get
  case getMemMapping scope memE of
    Just env -> return env
    Nothing ->
      throwError
        . EvalError
        $ "<Internal Memory error when fetching environment>"

getEnvForCurrentScope :: EStack Env
getEnvForCurrentScope = do
  scope <- ask
  getEnvForScope scope

getNewScope :: Env -> EStack Scope
getNewScope env = do
  (memC, memE) <- get
  let (newMemE, newScope) = insertMem env memE
  put (memC, newMemE)
  return newScope

insertEnv :: Identifier -> ComputationPtr -> Env -> Env
insertEnv k b = Env . M.insert k b . getMap

getVal :: Identifier -> Env -> Maybe ComputationPtr
getVal var = M.lookup var . getMap

-- Updates the memory map, and returns the added data's memory address
insertMem :: a -> Mem a -> (Mem a, MemAddress a)
insertMem v m =
  let i = getCounter m
      address = Addr i
      nextAddress = succ i
      newMap = M.insert address v . getMappings $ m
   in (Mem newMap nextAddress, address)

updateMem :: MemAddress a -> a -> Mem a -> Mem a
updateMem id v (Mem m i) = Mem newMap i where newMap = M.adjust (const v) id m

getMemMapping :: MemAddress a -> Mem a -> Maybe a
getMemMapping id = M.lookup id . getMappings

-- Setup for usage
-----------------------------------------------------------------

prepareDefaultBindings :: (Scope, Mem Env, Mem Computation)
prepareDefaultBindings =
  let defaultBindings :: [(T.Text, Val)]
      defaultBindings =
        [ ("+", createBinary safePlus),
          ("-", createBinary safeMinus),
          ("*", createBinary safeMult),
          ("/", createBinary safeDiv),
          ("string-append", createBinary safeAppend),
          ("cons", createBinary safeCons),
          ("and", createBinary safeAnd),
          ("or", createBinary safeOr),
          ("car", createUnary safeCar),
          ("cdr", createUnary safeCdr),
          ("zero?", createUnary zeroHuh),
          ("empty?", createUnary emptyHuh),
          ("fix", createUnary safeFix),
          ("not", createUnary safeNot),
          ("empty", E)
        ]
      (defaultEnv, memC) =
        foldr
          ( \(func, val) (e, m) ->
              let (newMem, newC) = insertMem (Computed val) m
               in (insertEnv (Var func) newC e, newMem)
          )
          (Env M.empty, Mem M.empty 0)
          defaultBindings
      (memE, _) = insertMem defaultEnv (Mem M.empty scopeAdr)
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
      v1 <- getMemoizedValue (Var "2")
      v2 <- getMemoizedValue (Var "1")
      binOp v1 v2

    createUnary unOp = createBuiltIn 1 $ do
      v1 <- getMemoizedValue (Var "1")
      unOp v1

    safePlus :: Val -> Val -> EStack Val
    safePlus (I n) (I m) = return . I $ n + m
    safePlus _ _ = throwError . EvalError $ "Expected two numbers"

    safeMinus :: Val -> Val -> EStack Val
    safeMinus (I n) (I m) = return . I $ n - m
    safeMinus _ _ = throwError . EvalError $ "Expected two numbers"

    safeMult :: Val -> Val -> EStack Val
    safeMult (I n) (I m) = return . I $ n * m
    safeMult _ _ = throwError . EvalError $ "Expected two numbers"

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
      -- we do the following to allow circular mapping:
      -- 1. store closure's body at memory address `M`
      -- 2. bind closure's identifier to `M`
      -- 3. evaluate body with the "fixed" environment mentioned above
      (memC, memE) <- get
      env <- getEnvForScope scope
      let (uMem, memId) = insertMem (Computation b scope) memC
          fixedEnv = insertEnv identifier memId env
      put (uMem, updateMem scope fixedEnv memE)
      local (const scope) $ evalM b
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
