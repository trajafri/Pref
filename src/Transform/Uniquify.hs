{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Transform.Uniquify where
-- This pass changes all variables so that there are no duplicates

import           Control.Monad.State
import           Control.Monad.Reader
import           Syntax.Exp
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Set                      as S

{-
Pass maintains occurance of each variable + an evironment.

Variable occurance are used to 'gensym' a new variable name,
environment is used to figure out what the variable was changed to.


For example:

(let ([x 2]
      [x (+ 1 x)]) x)

Here, we expect [x 2] to compile to [x.0 2], and [x (+ 1 x)] to [x.1 (+ 1 x.1)].

Now, let's body could either be x.0 or x.1. From our environment, we will
deduce that x should actually compile to x.2

If a variable is unbound, it is replaced by a value which could never be in the env,
i.e, a value that occurs -1 amount of times.
-}

type Env = M.Map T.Text Int

builtIns :: S.Set T.Text
builtIns = S.fromList
  [ "+"
  , "-"
  , "*"
  , "/"
  , "zero?"
  , "empty?"
  , "cons"
  , "car"
  , "cdr"
  , "string-append"
  , "fix"
  ]

genSym :: T.Text -> StateT (M.Map T.Text Int) (Reader Env) (T.Text, Int)
genSym v = do
  m <- get
  let mp = M.insertWith (const succ) v 0 m
  put mp
  case M.lookup v mp of
    Just i  -> return (v, i)
    Nothing -> undefined

uniquify :: Exp -> StateT (M.Map T.Text Int) (Reader Env) Exp
uniquify Empty  = return Empty
uniquify (Id v) = if S.member v builtIns
  then return (Id v)
  else do
    env <- ask
    case M.lookup v env of
      Nothing -> return . Id $ v <> ".-1"
      Just n  -> return . Id $ v <> "." <> (T.pack . show $ n)
uniquify n@(NLiteral _ ) = return n
uniquify s@(SLiteral _ ) = return s
uniquify b@(BLiteral _ ) = return b
uniquify (  Lambda vs b) = do
  vars    <- mapM genSym vs
  uniqueB <- local (\env -> foldl (\e (v, i) -> M.insert v i e) env vars)
    $ uniquify b
  return $ Lambda [ v <> "." <> (T.pack . show $ i) | (v, i) <- vars ] uniqueB
uniquify (If q t f) = If <$> uniquify q <*> uniquify t <*> uniquify f
uniquify (Let ps b) = do
  let (vs, es) = unzip ps
  vars    <- mapM genSym vs
  exps    <- mapM uniquify es
  uniqueB <- local (\env -> foldl (\e (v, i) -> M.insert v i e) env vars)
    $ uniquify b
  return $ Let
    [ (v <> "." <> (T.pack . show $ i), e) | ((v, i), e) <- zip vars exps ]
    uniqueB
uniquify (App rator rands) = App <$> uniquify rator <*> mapM uniquify rands
uniquify _                 = undefined
--uniquify (Def v     e    ) = do
--  (uV, i) <- genSym v
--  uniqueE <- local (M.insert v i) $ uniquify e
--  return $ Def (uV <> "." <> (T.pack . show $ i)) uniqueE

uniquifyProgram :: [Exp] -> Env -> StateT (M.Map T.Text Int) (Reader Env) [Exp]
uniquifyProgram []             _   = return []
uniquifyProgram (Def v e : es) env = do
  (uV, i) <- genSym v
  let newEnv = M.insert v i env
  uniqueE <- local (const newEnv) $ uniquify e
  rest    <- uniquifyProgram es newEnv
  return $ (Def (uV <> "." <> (T.pack . show $ i)) uniqueE : rest)
uniquifyProgram (e : es) env = do
  uniqueE <- local (const env) $ uniquify e
  rest    <- uniquifyProgram es env
  return $ uniqueE : rest

