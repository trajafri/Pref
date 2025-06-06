{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Transform.CPS where

import           Control.Monad.State
import           Data.DList
import           Data.List               hiding ( head
                                                , tail
                                                )
import qualified Data.Text                     as T
import           Prelude                 hiding ( head
                                                , tail
                                                )
import           Syntax.Exp

{- NOTE: This CPSer does not account for currying done
         by interpreter
-}

{- Based on the command line option, we can decide
   what collector to use
   Collector will be used to store function
   identifiers that are undefined in Pref,
   but could be built-in functions in some other language.
   This way, we can generate simple cpsed version of these
   functions (assuming they are *simple* in the target lang).
   Example: If we see zero?, we generate a simple cpsed version
   of zero?, i.e (define zero?k (lambda (x k) (k (zero? x))))
-}
data Collector = Unit | FreeAndScoped [(T.Text, Int)] [T.Text]

collect :: (T.Text, Int) -> Collector -> Collector
collect e@(var, _) c@(FreeAndScoped free scoped) =
  if elem e free || elem var scoped then c else FreeAndScoped (e : free) scoped
collect _ x = x

getFixedExp :: Exp -> Collector -> Exp
getFixedExp i@(Id txt) (FreeAndScoped free _) =
  if elem txt $ fmap fst free then Id $ txt <> "k" else i
getFixedExp ex _ = ex

getFreeVars :: Collector -> [(T.Text, Int)]
getFreeVars (FreeAndScoped free _) = free
getFreeVars _                      = []

updateVars :: [T.Text] -> Collector -> Collector
updateVars newVars (FreeAndScoped free oldVars) =
  FreeAndScoped free $ newVars <> oldVars
updateVars _ c = c

removeVars :: [T.Text] -> Collector -> Collector
removeVars oldVars (FreeAndScoped free newVars) =
  FreeAndScoped free $ newVars \\ oldVars
removeVars _ c = c



letToApp :: Exp -> Exp
letToApp (Let bindings b) =
  let (vars, vals) = unzip bindings in App (Lambda vars b) vals
letToApp x = x

{- Cpses every expression
   Assumption: Everything that would be in
               in the environment is cpsed.
               (i.e, every Id is cpsed)
   cpser handles the top level only.
   It only introduces continuation to expressions if needed -}
cpser :: Exp -> State Collector Exp
cpser i@(Id _)          = return i
cpser e@Empty           = return e
cpser n@(NLiteral _   ) = return n
cpser s@(SLiteral _   ) = return s
cpser b@(BLiteral _   ) = return b
cpser (  Lambda vars b) = do
  modify $ updateVars vars
  cpsedBody <- cpsExp b
  modify $ removeVars vars
  return $ Lambda (vars ++ ["k"]) cpsedBody
-- The way I am doing things, `if` ends up having three cases
cpser (If (App rator rands) thn els) = do
  cpsedThn <- cpser thn
  cpsedEls <- cpser els
  let finalExp arg = If arg cpsedThn cpsedEls
  extractCpsAppExp rator rands finalExp
cpser (If l@(Let _ _) thn els) = cpser (If (letToApp l) thn els)
cpser (If cond        thn els) = do
  cpsedThn <- cpser thn
  cpsedEls <- cpser els
  return $ If cond cpsedThn cpsedEls
{- This case requires something similar to the app case.
    I could just transform it into a lambda application
    and use App case out of the box -}
cpser l@(Let _     _    ) = cpser $ letToApp l
-- Application at top, so we apply `id` to the final result
cpser (  App rator rands) = extractCpsAppExp rator rands id
cpser (  Def v     b    ) = do
  modify $ updateVars [v]
  cpsedBody <- cpser b
  return $ Def v cpsedBody

{- When this is called, we are guarenteed to be in a function
   with an argument, "k" for the current continuation.
   It invokes the continuation provided by cpser in the lambda case.
-}
cpsExp :: Exp -> State Collector Exp
cpsExp i@(Id _)       = return $ App (Id "k") [i] -- apply k to value
cpsExp e@Empty        = return $ App (Id "k") [e] -- apply k to value
cpsExp n@(NLiteral _) = return $ App (Id "k") [n] -- apply k to value
cpsExp s@(SLiteral _) = return $ App (Id "k") [s] -- apply k to value
cpsExp b@(BLiteral _) = return $ App (Id "k") [b] -- apply k to value
cpsExp l@(Lambda _ _) = do
  cpsedLambda <- cpser l
  return $ App (Id "k") [cpsedLambda] -- lambda's are simple, apply k!!
cpsExp (If (App rator rands) thn els) = do
  cpsedThn <- cpsExp thn
  cpsedEls <- cpsExp els
  let finalExp arg = If arg cpsedThn cpsedEls
  extractCpsAppExp rator rands finalExp
cpsExp (If cond thn els) = do
  cpsedThn <- cpsExp thn
  cpsedEls <- cpsExp els
  return $ If cond cpsedThn cpsedEls
cpsExp l@(Let _ _) = cpsExp $ letToApp l
cpsExp (App rator rands) =
  extractCpsAppExp rator rands $ \arg -> App (Id "k") [arg]
cpsExp (Def _ _) = undefined --can't have definitions in a lambda yet

{--**| Alright things are gonna get nasty now |**--}
{- This type, when run, returns an application completely cpsed that's waiting
   on the body of its last continuation.
   The argument function is then given the last result value and it
   should return the final expression

   Example: In (a (b c) d), the final result is:
            (b c (lambda (res1) (a res1 d (lambda (res2) ______ ))))
   The function returned by AppCPSer takes in a function, is applied to
   "Id res2" and returns the body for the final continuation
   (that goes in place of ______ ).

  * Int is for the argument number (currently, it goes like arg0, arg1, arg2 ...)
  * [Exp] is for the final result of each exp. If something is CPSed, then
    it's final result will be some argn. -}
type K = ((Exp -> Exp) -> Exp)

type AppCPSer = State (Int, DList Exp, Collector) K

type AppCPSerResult = (K, (Int, DList Exp, Collector))

runAppCPSer :: Int -> DList Exp -> Collector -> AppCPSer -> AppCPSerResult
runAppCPSer i ls c = flip runState (i, ls, c)

getLastIndex :: AppCPSerResult -> Int
getLastIndex = (\(a, _, _) -> a) . snd

getLastArgHandler :: AppCPSerResult -> K
getLastArgHandler = fst

getCollection :: AppCPSerResult -> Collector
getCollection = thd . snd

thd :: (a, b, c) -> c
thd (_, _, c) = c

{- The following function CPSes the application case
    as shown in the example above -}
cpsApp :: [Exp] -> AppCPSer
cpsApp [] = do
  exps <- gets $ \(_, a, _) -> a
  i    <- gets $ \(a, _, _) -> a
  let finalResult = "arg" <> (T.pack . show $ i)
  let e           = head exps
  let es          = tail exps
  case e of
    Id x -> modify $ \(a, b, c) -> (a, b, collect (x, length es) c)
    _    -> return ()
  collection <- gets thd
  let adjustedE = getFixedExp e collection
  let finalExp handleResult =
        Lambda [finalResult] . handleResult . Id $ finalResult
  let finalFunc handleResult = App adjustedE $ es ++ [finalExp handleResult]
  modify $ \(a, b, c) -> (a, b, updateVars [finalResult] c)
  return finalFunc {- Here, we reconstruct the original application from the
                                 accumulated bindings for each expression in the application,
                                 create the last lambda, and wait for its body. -}
-- In this case, we cps the application with a new AppCPSer, and construct the whole
-- expression using its final values
cpsApp (App rator rands : exs) = do
  vars <- gets $ \(_, b, _) -> b
  modify $ \(a, _, c) -> (a, empty, c) -- This is because we want to start with a fresh list
  currExpCont <- cpsApp (rator : rands) -- CPS the application, and get the cont function
  modify $ \(a, _, c) -> (a, vars, c)
  i <- gets $ \(a, _, _) -> a
  modify $ \(a, b, c) -> (a, snoc b (Id ("arg" <> (T.pack . show $ i))), c) -- This is the result of the whole application
  modify $ \(a, b, c) -> ((const $ succ i) a, b, c) -- If argn was used by last, the next should start with arg(n+1)
  nextExpsCont <- cpsApp exs
  -- Now, currExpCont is waiting for the result of `exs`
  let nextExps handleCont = const $ nextExpsCont handleCont
  return $ currExpCont . nextExps
{- result of nextExps becomes the body of the last continuation of (App rator rands)! -}
cpsApp (simpleExp : exs) = do
  collection <- gets thd
  let (cpsedSimpleExp, updatedCollection) =
        flip runState collection $ cpser simpleExp
  modify $ \(a, b, _) -> (a, b, updatedCollection)
  modify $ \(a, b, c) -> (a, snoc b cpsedSimpleExp, c)-- The result is the expression cpsed
  cpsApp exs

extractCpsAppExp :: Exp -> [Exp] -> (Exp -> Exp) -> State Collector Exp
extractCpsAppExp rator rands handleFinalArg = do
  c <- get
  let appCpser = runAppCPSer 0 empty c . cpsApp $ rator : rands
  put $ getCollection appCpser
  return . ($ handleFinalArg) . getLastArgHandler $ appCpser
