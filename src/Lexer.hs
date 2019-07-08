{-# LANGUAGE OverloadedStrings, ViewPatterns  #-}

module Lexer
  ( decimal
  , identifier
  , parens
  , stringLiteral
  , tokenize
  , whiteSpace
  )
where

import           Control.Monad.Identity
import           Data.Char
import qualified Data.Text                     as T
import           Errors
import           Syntax.Tokens
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

tokenize :: T.Text -> Either Error [Token]
tokenize = lexer
 where
  lexer :: T.Text -> Either Error [Token]
  lexer (T.uncons -> Nothing) = return []
  lexer (T.uncons -> Just ('(' , ts)) =
    lexer ts >>= \res -> return $ LParen : res
  lexer (T.uncons -> Just (')' , ts)) =
    lexer ts >>= \res -> return $ RParen : res
  lexer (T.uncons -> Just (c , ts))
    | isDigit c = do
      let (num, rest) = T.span isDigit ts
      res <- lexer rest
      return $ ID (T.cons c num) : res
    | validIdChar c = do
      let (rId, rest) = T.span validIdChar ts
      res <- lexer rest
      return $ ID (T.cons c rId) : res
    | c == '\"' --The only reason to make lexing hard :(
                = do
      let (str, rest) = T.span (/= '\"') ts
      case (T.uncons rest) of
        Just ('\"', rrest) -> do
          res <- lexer rrest
          return $ ID (flip T.snoc c $ T.cons c str) : res
        _ ->
          Left
            . LexerError
            $ "LexerError: A string was not terminated with a quote."
    | otherwise = lexer ts --Unidentified character
  lexer x =
    Left . LexerError $ "LexerError: Encountered an expected text: " <> x


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
