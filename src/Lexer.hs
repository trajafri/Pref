{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( decimal
  , identifier
  , parens
  , stringLiteral
  , whiteSpace
  )
where

import           Control.Monad.Identity
import           Data.Char
--import qualified Data.Text                     as T
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as PT

{- Regarding using the "user state" in ParsecT:
   If we were to stack ParsecT on some monad M, the
   difference would be that ParsecT's state
   (aka "user state") also backtracks on failure, but
   the effects of M do not!
   Imagine backtracking after performing IO. The IO
   effect would should up even if ParsecT backtracked.
-}

prefDefinition :: LanguageDef ()
prefDefinition = emptyDef { commentLine   = ";"
                          , commentStart  = "|#"
                          , commentEnd    = "|#"
                          , caseSensitive = True
                          }

lexer :: PT.GenTokenParser String () Identity
lexer = PT.makeTokenParser prefDefinition

identifier :: Parsec String () String
identifier = do
  idName@(idC : ids) <- many1 (alphaNum <|> satisfy validIdChar)
  if isDigit idC && all isDigit ids then mzero else return idName

parens :: Parsec String () a -> Parsec String () a
parens = PT.parens lexer

stringLiteral :: Parsec String () String
stringLiteral = PT.stringLiteral lexer

decimal :: Parsec String () Integer
decimal = PT.decimal lexer

whiteSpace :: Parsec String () ()
whiteSpace = PT.whiteSpace lexer

validIdChar :: Char -> Bool
validIdChar c = all
  ($ c)
  [ not . isSpace
  , (')' /=)
  , ('(' /=)
  , ('\"' /=)
  , ('\'' /=)
  , ('.' /=)
  , (',' /=)
  , ('[' /=)
  , (']' /=)
  , ('{' /=)
  , ('}' /=)
  , ('<' /=)
  , ('>' /=)
  , ('#' /=)
  , ('`' /=)
  , (':' /=)
  , (';' /=)
  ]
