module Transform.CPS where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Functor.Identity
import Exp

-- TODO, maybe fix reverse by DList?
{- Cpses every expression
   Assumption: Everything that would be in
               in the environment is cpsed.
               (i.e, every Id is cpsed)
   cpser handles the top level only.
   It only introduces continuation to expressions if needed -}
cpser :: Exp -> Exp
cpser i@(Id _) = i
cpser n@(NLiteral _) = n
cpser s@(SLiteral _) = s
cpser (Lambda vars b) = (Lambda (vars ++ ["k"]) $ cpsExp b)
-- The way I am doing things, `if` ends up having two cases
cpser (If (App rator rands) thn els) =
  let finalExp = \arg -> If arg (cpser thn) (cpser els)
   in extractCpsAppExp rator rands finalExp
cpser (If cond thn els) = If cond (cpser thn) (cpser els)
{- This case requires something similar to the app case.
    I could just transform it into a lambda application
    and use App case out of the box -}
cpser (Let bindings b) =
  let vars = map fst bindings
      vals = map snd bindings
   in cpser (App (Lambda vars b) vals)
-- Application at top, so we apply `id` to the final result
cpser (App rator rands) = extractCpsAppExp rator rands $ id
cpser (Def v b) = (Def v (cpser b))

{- When this is called, we are guarenteed to be in a function
   with an argument, "k" for the current continuation.
   It invokes the continuation provided by cpser in the lambda case. -}
cpsExp :: Exp -> Exp
cpsExp i@(Id _) = App (Id "k") [i] -- apply k to value
cpsExp n@(NLiteral _) = App (Id "k") [n] -- apply k to value
cpsExp s@(SLiteral _) = App (Id "k") [s] -- apply k to value
cpsExp l@(Lambda _ _) = App (Id "k") [cpser l] -- lambda's are simple, apply k!!
cpsExp (If (App rator rands) thn els) =
  let finalExp = \arg -> If arg (cpsExp thn) (cpsExp els)
   in extractCpsAppExp rator rands finalExp
cpsExp (If cond thn els) = If cond (cpsExp thn) (cpsExp els)
cpsExp (Let bindings b) =
  let vars = map fst bindings
      vals = map snd bindings
   in cpsExp (App (Lambda vars b) vals)
cpsExp (App rator rands) =
  extractCpsAppExp rator rands $ \arg -> App (Id "k") [arg]
cpsExp (Def _ _) = undefined --Language can't have definitions in a lambda

{--**| Alright things are gonna get nasty now |**--}
{- This type, when run, returns an application completely cpsed that's waiting
   on the body of its last continuation.
   The argument function is then given the last result value and it
   should return the final expression

   Example: In (a (b c) d), the final result is:
            (b c (lambda (res1) (a res1 d (lambda (res2) ______ ))))
   The function returned by AppCPSer takes in a function, is applied to
   "Id res2" and returns the body for the final continuation
   (that goes in place of ________).

  * Int is for the argument number (currently, it goes like arg0, arg1, arg2 ...)
  * [Exp] is for the final result of each exp. If something is CPSed, then
    it's final result will be some argn. -}
type AppCPSer = StateT Int (StateT [Exp] Identity) ((Exp -> Exp) -> Exp)

type AppCPSerResult = (((Exp -> Exp) -> Exp, Int), [Exp])

runAppCPSer :: Int -> [Exp] -> AppCPSer -> AppCPSerResult
runAppCPSer i ls = runIdentity . (`runStateT` ls) . (`runStateT` i)

getLastIndex :: AppCPSerResult -> Int
getLastIndex = snd . fst

getLastArgHandler :: AppCPSerResult -> ((Exp -> Exp) -> Exp)
getLastArgHandler = fst . fst

{- The following function CPSes the application case
    as shown in the example above -}
cpsApp :: [Exp] -> AppCPSer
cpsApp [] {- Here, we reconstruct the original application from the
             accumulated bindings for each expression in the application,
             create the last lambda, and wait for its body. -}
 = do
  exps <- lift $ get
  i <- get
  let finalResult = "arg" ++ show i
  let fixedExps = reverse exps -- since we were consing items, gotta reverse here
  let finalExp handleResult =
        Lambda [finalResult] . handleResult . Id $ finalResult
  let finalFunc handleResult =
        App (head fixedExps) $ tail fixedExps ++ [finalExp handleResult]
  return $ finalFunc
cpsApp (App rator rands:exs) {- In this case, we cps the application with
                                a new AppCPSer, and construct the whole
                                expression using its final values -}
 = do
  count <- get
  let ((currExpCont, i), _) = (runAppCPSer count []) . cpsApp $ (rator : rands)
  lift . modify $ ((Id $ "arg" ++ show i) :) -- This the result of the whole application
  modify (const $ succ i) -- If argn was used by last, the next should start with arg(n+1)
  nextExpsCont <- cpsApp exs
  -- Now, currExpCont is waiting for the result of `exs`
  let nextExps handleCont = \_ -> nextExpsCont handleCont
  return $ currExpCont . nextExps {- result of nextExps becomes the body of
                                     the last continuation of (App rator rands)! -}
cpsApp (simpleExp:exs) = do
  lift . modify $ (simpleExp :) -- The result is the expression itself
  cpsApp exs

extractCpsAppExp :: Exp -> [Exp] -> (Exp -> Exp) -> Exp
extractCpsAppExp rator rands handleFinalArg =
  ($ handleFinalArg) . getLastArgHandler . (runAppCPSer 0 []) . cpsApp $
  (rator : rands)
