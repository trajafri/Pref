{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Text as T
import Options.Applicative
import qualified Pref as P
import Syntax.Exp
import Transform.CPS

cliParser :: Parser (Bool, String)
cliParser =
  (,)
    <$> switch
      ( long "defineFreeFuncs"
          <> short 'd'
          <> help
            "Define CPS versions of free functions found in the target program"
      )
    <*> argument
      str
      ( metavar "TARGET"
          <> help "Path to target pref program that needs to be transformed"
      )

cps :: (Bool, String) -> IO ()
cps (defineFree, fp) = do
  let collector = if defineFree then FreeAndScoped [] [] else Unit
  fileContent <- readFile fp
  ast <-
    either (const $ fail "Provided file should be syntatically correct.") return $
      P.codeToAst (T.pack fileContent)
  let (cpsedExps, collection) = flip runState collector $ mapM cpser ast
  let cpsedFile = intercalate "\n" $ map show cpsedExps
  let definitions =
        intercalate "\n" $
          map (show . defineFreeVar) (getFreeVars collection)
            <> ["\n"]
  putStr $ definitions <> cpsedFile
  where
    defineFreeVar :: (Identifier, Int) -> Exp
    defineFreeVar (Var var, arity) =
      let vars = map (Var . T.pack . ("var" <>) . show) [1 .. arity]
       in Def (Var $ var <> "k") $
            Lambda
              (vars <> [Var "k"])
              [ App
                  (Id . Var $ "k")
                  [App (Id . Var $ var) $ map Id vars]
              ]

main :: IO ()
main = execParser opts >>= cps
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "CPS the given TARGET"
            <> header
              "cps - CPS a target pref program"
        )
