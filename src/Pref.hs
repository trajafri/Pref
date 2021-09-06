{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Pref
  ( codeToAst
  , codeToVal
  , prepareDefaultBindings
  , eval
  , evaluatePref
  , Env(..)
  , Val(..)
  , Box(..)
  , PrefComputation(PrefE)
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

-- Type Setup
-----------------------------------------------------------------

-- `v` represents what a variable is bound to.
-- This will help when we try to generalize the interpreter to
-- implement any evaluation strategy.
data Env v = Env {getMap :: Map T.Text v} deriving (Eq, Show)

-- `d` represents the data that a memory address points to.
data Mem d = Mem (Map Int d) -- ^ mapping from memory-address to data
                 Int         -- ^ memory-address counter

-- `v` is the type level argument for environments/values
-- Box represents a computation in a lazy interpreter
data Box v = Computation Exp (Env v) -- ^ a value that hasn't been computed yet,
           | Computed (Val v)        -- ^ a value that has been computed before
  deriving (Eq, Show)

-- Interpreter can see two kinds of computations
-- 1. An expression written in Pref
-- 2. A program written in haskell by the interpreter.
--    These programs are used to implement built-in functions
data PrefComputation = PrefE Exp | InterpE (EStack MemVal)

-- `Eq` is implemented only for testing purposes
-- Since there's no point to testing two partial haskell
-- computations, all cases with `InterpE` are not equal
instance Eq PrefComputation where
  (PrefE exp1) == (PrefE exp2) = exp1 == exp2
  _            == _            = False

-- `v` is the type level argument for the environment.
-- For Call-by value, this is simply `Val`
data Val v
  = S T.Text
  | I Int
  | B Bool
  | C T.Text PrefComputation (Env v) -- ^ Closure
                                     -- Note that a closure doesn't contain an `Exp`
  | T Exp (Env v)        -- ^ Thunk
  | Cons (Val v) (Val v)
  | E                    -- ^ Empty list
  deriving Eq

instance Show (Val d) where
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

-- Value with environment
type MemAddress = Int
type MemVal = Val MemAddress

-- interpreter's monad stack
type EStack val
  = StateT (Mem (Box Int)) (ReaderT (Env Int) (Either EvalError)) val

-- Interpreter
--------------------------------------------------------------------

eval :: Exp -> Env Int -> Mem (Box Int) -> Either EvalError MemVal
eval e env mem = (`runReaderT` env) . (`evalStateT` mem) . evalM $ e

evalM :: Exp -> EStack MemVal
evalM Empty                 = return E     -- EmptyList 
evalM (SLiteral s         ) = return $ S s -- Strings
evalM (NLiteral i         ) = return $ I i -- Numbers
evalM (BLiteral b         ) = return $ B b -- Bools
evalM (Id       identifier) = getMemoizedValue identifier
evalM (Lambda [] body     ) = do -- Thunk case
  env <- ask
  return $ T body env
evalM (Lambda (identifier : []) body) = do -- Lambda base case
  env <- ask
  return $ C identifier (PrefE body) env
evalM (Lambda (identifier : ids) body) = do -- Lambda currying case
  let curriedLambda = Lambda [identifier] $ Lambda ids body
  evalM curriedLambda
evalM (Let bindings body) = do -- Let case
  env        <- ask
  updatedEnv <- foldM pushToEnv env bindings
  local (const updatedEnv) $ evalM body
 where
  pushToEnv :: (Env Int) -> (T.Text, Exp) -> EStack (Env Int)
  pushToEnv newEnv (identifier, exp) = do
    memAdd <- memoize exp
    return $ insertEnv identifier memAdd newEnv
evalM (If cond thn els) = do -- If case
  eCond <- evalM cond
  case eCond of
    (B False) -> evalM els
    _         -> evalM thn
evalM (App rator []) = do -- Thunk application
  ratorVal <- evalM rator
  case ratorVal of
    (T body env) -> local (const env) $ evalM body
    _            -> throwBadApplicationError
evalM (App rator (rand : rands)) = do -- Function application
  ratorVal <- evalM rator
  applyClosure ratorVal rand rands
evalM (Def _ _) =
  throwError
    . EvalError
    $ "A non-top level `defined` expression is not supported"

-- Utilities
--------------------------------------------------------------------

applyClosure :: MemVal -> Exp -> [Exp] -> EStack MemVal
applyClosure (C identifier body env) rand remainingRands = do
  memAddress <- memoize rand
  let localEnv = insertEnv identifier memAddress env
  -- Note: the variable below just tells us how to run the computation
  --       to evaluate the closure's body. It doesn't actually run it
  let evalBody = local (const localEnv) $ case body of
        (PrefE   exp ) -> evalM exp
        (InterpE comp) -> comp
  case remainingRands of
    []       -> evalBody -- no more arguments, so just return whatever body returns
    (r : rs) -> do       -- body better return a closure. Apply it to rest of rands
      clos <- evalBody
      applyClosure clos r rs
applyClosure _ _ _ =
  throwError
    . EvalError
    $ "Bad application\nA non-function was applied like a function?"

-- Given a variable, if its value is already computed, simply return it,
-- Else, compute it, memoize it, and return it
getMemoizedValue :: T.Text -> EStack MemVal
getMemoizedValue identifier = do
  memAddress <- resolveIdentifier identifier
  box        <- gets $ getMemMapping memAddress
  case box of
    Just (Computation exp oldEnv) -> do -- memoize computed value
      val <- local (const oldEnv) $ evalM exp
      modify $ updateMem memAddress (Computed val)
      return val
    Just (Computed value) -> return value
    Nothing -> throwError . EvalError $ "Memory error " <> identifier

-- If given a variable, get's the memory address for the value it points to
-- Else, places the exp in the memory table and returns its memory address
-- This should be used whenever a *variable is bound to a value* to stay lazy
memoize :: Exp -> EStack MemAddress
memoize (Id var) = do
  -- A bound variable, therefore it's a computation we have seen before.
  -- be careful and make sure the computation isn't evaluated (to stay lazy)
  memAddress <- resolveIdentifier var
  return memAddress
memoize exp = do
  currEnv                       <- ask
  (updatedMemTable, memAddress) <- gets $ insertMem (Computation exp currEnv)
  put updatedMemTable
  return memAddress

resolveIdentifier :: T.Text -> EStack Int
resolveIdentifier identifier = do
  env <- ask
  case getVal identifier env of
    Just v  -> return v
    Nothing -> throwUnboundVariableError identifier

insertEnv :: T.Text -> d -> Env d -> Env d
insertEnv k b = Env . M.insert k b . getMap

getVal :: T.Text -> Env v -> Maybe v
getVal var = M.lookup var . getMap

-- Updates the memory map, and returns the added data's memory address
insertMem :: a -> Mem a -> (Mem a, Int)
insertMem v (Mem m i) =
  let nextAddress = succ i
      newMap      = (M.insert i v m)
  in  (Mem newMap nextAddress, i)

updateMem :: Int -> a -> Mem a -> Mem a
updateMem id v (Mem m i) = Mem newMap i where newMap = M.adjust (const v) id m

getMemMapping :: Int -> Mem a -> Maybe a
getMemMapping id (Mem m _) = M.lookup id m

throwUnboundVariableError :: T.Text -> (EStack v)
throwUnboundVariableError identifier =
  throwError . EvalError $ "Can not identify variable '" <> identifier <> "'"

throwBadApplicationError :: (EStack MemVal)
throwBadApplicationError =
  throwError
    . EvalError
    $ "Bad application\nPerhaps a function was applied to too many arguments?"

-- Setup for usage
-----------------------------------------------------------------

prepareDefaultBindings :: (Env Int, Mem (Box Int))
prepareDefaultBindings =
  let defaultBindings =
          [ ("+"            , createBinary safePlus)
          , ("-"            , createBinary safeMinus)
          , ("*"            , createBinary safeMult)
          , ("/"            , createBinary safeDiv)
          , ("string-append", createBinary safeAppend)
          , ("cons"         , createBinary safeCons)
          , ("car"          , createUnary safeCar)
          , ("cdr"          , createUnary safeCdr)
          , ("zero?"        , createUnary zeroHuh)
          , ("empty?"       , createUnary emptyHuh)
          , ("fix"          , createUnary safeFix)
          ]
      (defaultEnv, memoryTable) = Prelude.foldr
        (\(func, val) (e, m) ->
          let (newMem, newC) = insertMem (Computed val) m
          in  (insertEnv func newC e, newMem)
        )
        (Env M.empty, Mem M.empty 0)
        defaultBindings
      updateMemTable (Mem table counter) = Mem
        (M.map
          (\case
            (Computed (C v b _)) -> Computed $ C v b defaultEnv
            v                    -> v
          )
          table
        )
        counter
  in  (defaultEnv, updateMemTable memoryTable)
 where
  createBuiltIn :: Int -> (EStack MemVal) -> MemVal
  createBuiltIn m comp = compute m (Env M.empty)
   where
    compute 1 = C "1" (InterpE comp)
    compute n = C
      identifier
      (InterpE $ do
        env <- ask
        return . compute (pred n) $ env
      )
      where identifier = (T.pack . show $ n)

  createBinary binOp = createBuiltIn 2 $ do
    v1 <- getMemoizedValue "2"
    v2 <- getMemoizedValue "1"
    binOp v1 v2


  createUnary unOp = createBuiltIn 1 $ do
    v1 <- getMemoizedValue "1"
    unOp v1


  safePlus :: MemVal -> MemVal -> EStack MemVal
  safePlus (I n) (I m) = return . I $ n + m
  safePlus _     _     = throwError . EvalError $ "Expected two numbers"

  safeMinus :: MemVal -> MemVal -> EStack MemVal
  safeMinus (I n) (I m) = return . I $ n - m
  safeMinus _     _     = throwError . EvalError $ "Expected two numbers"

  safeMult :: MemVal -> MemVal -> EStack MemVal
  safeMult (I n) (I m) = return . I $ n * m
  safeMult _     _     = throwError . EvalError $ "Expected two numbers"

  safeDiv :: MemVal -> MemVal -> EStack MemVal
  safeDiv (I n) (I m) = return . I $ n `div` m
  safeDiv _     _     = throwError . EvalError $ "Expected two numbers"

  safeAppend :: MemVal -> MemVal -> EStack MemVal
  safeAppend (S a) (S b) = return . S $ a <> b
  safeAppend _     _     = throwError . EvalError $ "Expected two strings"

  safeCons :: MemVal -> MemVal -> EStack MemVal
  safeCons a b = return $ Cons a b

  safeCar :: MemVal -> EStack MemVal
  safeCar (Cons a _) = return a
  safeCar _          = throwError . EvalError $ "Expected a list"

  safeCdr :: MemVal -> EStack MemVal
  safeCdr (Cons _ b) = return b
  safeCdr _          = throwError . EvalError $ "Expected a list"

  zeroHuh :: MemVal -> EStack MemVal
  zeroHuh (I 0) = return $ B True
  zeroHuh _     = return $ B False

  emptyHuh :: MemVal -> EStack MemVal
  emptyHuh E = return $ B True
  emptyHuh _ = return $ B False

  safeFix :: MemVal -> EStack MemVal
  safeFix (C identifier (PrefE b) env) = do
    -- we do the following to allow circular mapping:
    -- * get the memory address `M` bound to "1"
    -- * create a closure with environment that maps `identifier` to itself (at `M`)
    -- * place closure at `M`
    -- * evaluate the body
    --env <- ask
    --let selfApp = App (Id "1") [Id "x"]
    --(uMem, memId) <- gets $ insertMem (Computation selfApp env)
    --put uMem
    --let fixedEnv = insertEnv "x" memId env
    --modify $ updateMem memId (Computation selfApp fixedEnv)
    --local (const fixedEnv) $ evalM selfApp
    (uMem, memId) <- gets $ insertMem (Computation b env)
    put uMem
    let fixedEnv = insertEnv identifier memId env
    modify $ updateMem memId (Computation b fixedEnv)
    local (const fixedEnv) $ evalM b


  safeFix _ = throwError . EvalError $ "fix expects a non-zero arity function"


--TODO: Memory can be updated by top-level expressions in cbr
evalList
  :: [Exp]
  -> [(T.Text, Exp)]
  -> Env Int
  -> Mem (Box Int)
  -> Either EvalError [MemVal]
evalList [] _ _ _ = return []
evalList (Def id binding : es) futureBindings env mem =
  let newFutures    = L.drop 1 futureBindings
      fixedBinding  = topLevelFunction id newFutures binding
      (uMem, memId) = insertMem (Computation fixedBinding env) mem
      val           = memId
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

codeToVal :: T.Text -> Either EvalError (Either ParseError [MemVal])
codeToVal code = case codeToAst code of
  Left  e   -> return . Left $ e
  Right ast -> case evalList ast (futureBindings ast) defaultEnv defaultMem of
    Left  e    -> Left e
    Right vals -> return . Right $ vals
 where
  futureBindings ast = [ (i, b) | (Def i b) <- ast ]
  (defaultEnv, defaultMem) = prepareDefaultBindings

evaluatePref :: T.Text -> T.Text
evaluatePref =
  either (T.pack . show) (either (T.pack . show) (T.pack . show)) . codeToVal
