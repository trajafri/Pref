{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State
import           Data.List
import qualified Data.Text                     as T
import           Options.Applicative
import qualified Pref                          as P
import           Syntax.Exp
import           Transform.CPS

cliParser :: Parser (Bool, String)
cliParser =
  (,)
    <$> switch
          (  long "defineFreeFuncs"
          <> short 'd'
          <> help
               "Define CPS versions of free functions found in the target program"
          )
    <*> argument
          str
          (  metavar "TARGET"
          <> help "Path to target pref program that needs to be transformed"
          )

cps :: (Bool, String) -> IO ()
cps (defineFree, fp) = do
  let collector = if defineFree then FreeAndScoped [] [] else Unit
  fileContent <- readFile fp
  ast         <-
    either (const $ fail "Provided file should be syntatically correct.") return
      $ P.codeToAst (T.pack fileContent)
  let (cpsedExps, collection) = flip runState collector $ mapM cpser ast
  let cpsedFile               = intercalate "\n" $ map show cpsedExps
  let definitions =
        intercalate "\n"
          $  map (show . defineFreeVar) (getFreeVars collection)
          <> ["\n"]
  putStr $ definitions <> cpsedFile

 where
  defineFreeVar :: (T.Text, Int) -> Exp
  defineFreeVar (var, arity) =
    let vars = map (T.pack . ("var" <>) . show) [1 .. arity]
    in  Def (var <> "k") $ Lambda (vars <> ["k"]) $ App
          (Id "k")
          [App (Id var) $ map Id vars]


main :: IO ()
main = execParser opts >>= cps
 where
  opts = info
    (cliParser <**> helper)
    (fullDesc <> progDesc "CPS the given TARGET" <> header
      "cps - CPS a target pref program"
    )

