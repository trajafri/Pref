{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                     as T
import           Options.Applicative
import qualified Pref                          as P

prefParser :: Parser String
prefParser = argument
  str
  (  metavar "SOURCE"
  <> help "Path to source pref program that needs to be evaluated"
  )

eval :: String -> IO ()
eval sourcePath = do
  fileContent <- readFile sourcePath
  maybeAst    <- return $ P.codeToVal (T.pack fileContent)
  putStr $ either show (either show $ unwords . map show) maybeAst
  putStrLn ""

main :: IO ()
main = execParser opts >>= eval
 where
  opts = info
    (prefParser <**> helper)
    (fullDesc <> progDesc "Evaluate the given SOURCE" <> header
      "pref - Evaluate a target Pref program"
    )

