{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State
import           Data.List
import qualified Data.Text                     as T
import qualified Pref                          as P
import           System.Console.ArgParser
import           Syntax.Exp
import           Transform.CPS

cliParser :: ParserSpec (String, String, Bool)
cliParser =
  (,,)
    `parsedBy` reqPos "filePath"
    `andBy`    reqPos "outFilePath"
    `andBy`    boolFlag "defineFree"

cps :: (String, String, Bool) -> IO ()
cps (fp, op, defineFree) = do
  let collector = if defineFree then FreeAndScoped [] [] else Unit
  fileContent <- readFile fp
  ast <- either (fail "Provided file should be syntatically correct.") return
    $ P.codeToAst (T.pack fileContent)
  let (cpsedExps, collection) = flip runState collector $ mapM cpser ast
  let cpsedFile               = intercalate "\n" $ map show cpsedExps
  let definitions =
        intercalate "\n"
          $  map (show . defineFreeVar) (getFreeVars collection)
          <> ["\n"]
  writeFile op $ definitions <> cpsedFile

 where
  defineFreeVar :: (T.Text, Int) -> Exp
  defineFreeVar (var, arity) =
    let vars = map (T.pack . ("var" <>) . show) [1 .. arity]
    in  Def (var <> "k") $ Lambda (vars <> ["k"]) $ App
          (Id "k")
          [App (Id $ var) $ map Id vars]


main :: IO ()
main = withParseResult cliParser cps

