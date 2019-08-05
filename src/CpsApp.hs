{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State
import           Data.List
import qualified Data.Text                     as T
import qualified Pref                          as P
import           System.Console.ArgParser
import           Syntax.Exp
import           Transform.CPS

data FreeAndScoped = FAS [(T.Text, Int)] [T.Text]
instance Collector FreeAndScoped where
  collect e@(var, _) c@(FAS free scoped) =
    if elem e free || elem var scoped then c else FAS (e : free) scoped

  getFixedExp i@(Id txt) (FAS free _) =
    if elem txt $ map fst free then Id $ txt <> "k" else i
  getFixedExp ex _ = ex

  getFreeVars (FAS free _) = free

  updateVars newVars (FAS free oldVars) = FAS free $ newVars <> oldVars

  removeVars oldVars (FAS free newVars) = FAS free $ newVars \\ oldVars

cliParser :: ParserSpec (String, String, Bool)
cliParser =
  (,,)
    `parsedBy` reqPos "filePath"
    `andBy`    reqPos "outFilePath"
    `andBy`    boolFlag "defineFree"

cps :: (String, String, Bool) -> IO ()
cps (fp, op, defineFree) = do
  let collector = FAS [] []
  fileContent <- readFile fp
  ast <- either (fail "Provided file should be syntatically correct.") return
    $ P.codeToAst (T.pack fileContent)
  let (cpsedExps, FAS free _) = flip runState collector $ mapM cpser ast
  let cpsedFile               = intercalate "\n" $ map show cpsedExps
  let definitions = if defineFree
        then intercalate "\n" $ map (show . defineFreeVar) free <> ["\n"]
        else ""
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

