{-# LANGUAGE OverloadedStrings #-}

module Transform.CPS where

import           Control.Monad.Stack.State
import           Control.Monad.State
import           Data.DList
import           Data.Functor.Identity
import qualified Data.Text                     as T
import           Prelude                 hiding ( head
                                                , tail
                                                )
import           Syntax.Exp

{- NOTE: This CPSer does not account for currying done
         by interpreter
-}
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
cpser :: Exp -> Exp
cpser i@(Id       _   ) = i
cpser n@(NLiteral _   ) = n
cpser s@(SLiteral _   ) = s
cpser (  Lambda vars b) = Lambda (vars ++ ["k"]) $ cpsExp b
-- The way I am doing things, `if` ends up having three cases
cpser (If (App rator rands) thn els) =
  let finalExp arg = If arg (cpser thn) (cpser els)
  in  extractCpsAppExp rator rands finalExp
cpser (  If l@(Let _ _) thn els) = cpser (If (letToApp l) thn els)
cpser (  If cond        thn els) = If cond (cpser thn) (cpser els)
{- This case requires something similar to the app case.
    I could just transform it into a lambda application
    and use App case out of the box -}
cpser l@(Let _     _           ) = cpser $ letToApp l
-- Application at top, so we apply `id` to the final result
cpser (  App rator rands       ) = extractCpsAppExp rator rands id
cpser (  Def v     b           ) = Def v (cpser b)

{- When this is called, we are guarenteed to be in a function
   with an argument, "k" for the current continuation.
   It invokes the continuation provided by cpser in the lambda case. -}
cpsExp :: Exp -> Exp
cpsExp i@(Id       _) = App (Id "k") [i] -- apply k to value
cpsExp n@(NLiteral _) = App (Id "k") [n] -- apply k to value
cpsExp s@(SLiteral _) = App (Id "k") [s] -- apply k to value
cpsExp l@(Lambda _ _) = App (Id "k") [cpser l] -- lambda's are simple, apply k!!
cpsExp (If (App rator rands) thn els) =
  let finalExp arg = If arg (cpsExp thn) (cpsExp els)
  in  extractCpsAppExp rator rands finalExp
cpsExp (  If cond thn els) = If cond (cpsExp thn) (cpsExp els)
cpsExp l@(Let _ _        ) = cpsExp $ letToApp l
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
type AppCPSer = StateT Int (StateT (DList Exp) Identity) ((Exp -> Exp) -> Exp)

type AppCPSerResult = (((Exp -> Exp) -> Exp, Int), DList Exp)

runAppCPSer :: Int -> DList Exp -> AppCPSer -> AppCPSerResult
runAppCPSer i ls = runIdentity . (`runStateT` ls) . (`runStateT` i)

getLastIndex :: AppCPSerResult -> Int
getLastIndex = snd . fst

getLastArgHandler :: AppCPSerResult -> ((Exp -> Exp) -> Exp)
getLastArgHandler = fst . fst

{- The following function CPSes the application case
    as shown in the example above -}
cpsApp :: [Exp] -> AppCPSer
cpsApp [] = do
  exps <- liftState get
  i    <- get
  let finalResult = "arg" <> (T.pack . show $ i)
  let e           = head exps
  let es          = toList . tail $ exps
  let finalExp handleResult =
        Lambda [finalResult] . handleResult . Id $ finalResult
  let finalFunc handleResult = App e $ es ++ [finalExp handleResult]
  return finalFunc {- Here, we reconstruct the original application from the
             accumulated bindings for each expression in the application,
             create the last lambda, and wait for its body. -}
cpsApp (App rator rands : exs) = do
  count <- get
  let ((currExpCont, i), _) =
        runAppCPSer count empty . cpsApp $ (rator : rands)
  liftState . modify $ (flip snoc $ Id ("arg" <> (T.pack . show $ i))) -- This the result of the whole application
  modify (const $ succ i) -- If argn was used by last, the next should start with arg(n+1)
  nextExpsCont <- cpsApp exs
  -- Now, currExpCont is waiting for the result of `exs`
  let nextExps handleCont = const $ nextExpsCont handleCont
  return $ currExpCont . nextExps {- In this case, we cps the application with
                                a new AppCPSer, and construct the whole
                                expression using its final values -} {- result of nextExps becomes the body of
                                     the last continuation of (App rator rands)! -}
cpsApp (simpleExp : exs) = do
  liftState . modify $ (flip snoc $ cpser simpleExp) -- The result is the expression cpsed
  cpsApp exs

extractCpsAppExp :: Exp -> [Exp] -> (Exp -> Exp) -> Exp
extractCpsAppExp rator rands handleFinalArg =
  ($ handleFinalArg)
    . getLastArgHandler
    . runAppCPSer 0 empty
    . cpsApp
    $ (rator : rands)
