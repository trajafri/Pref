module Main where

import           Data.List
import qualified Data.Text                     as T
import qualified Pref                          as P
import           System.Console.ArgParser
import           Transform.CPS

cliParser :: ParserSpec (String, String)
cliParser = (,) `parsedBy` reqPos "filePath" `andBy` reqPos "outFilePath"

cps :: (String, String) -> IO ()
cps (fp, op) = do
  fileContent <- readFile fp
  ast <- either (fail "Provided file should be syntatically correct.") return
    $ P.codeToAst (T.pack fileContent)
  writeFile op $ intercalate "\n" $ map (show . cpser) ast

main :: IO ()
main = withParseResult cliParser cps
