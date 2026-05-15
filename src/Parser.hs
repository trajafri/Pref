{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parse,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Lexer
import Syntax.Exp
import Text.Parsec hiding (parse)
import Prelude hiding
  ( exp,
    id,
  )

variableParser :: Parsec T.Text () Identifier
variableParser = Var <$> identifier

parse :: Parsec T.Text () [Exp]
parse =
  many $
    whiteSpace >> (defineParser <|> expParser <|> failIfRight) >>= \exp ->
      whiteSpace >> return exp
  where
    defineParser :: Parsec T.Text () Exp
    defineParser = try . parens $ do
      define <- identifier
      whiteSpace
      case define of
        "define" -> do
          ident <- variableParser
          whiteSpace
          Def ident <$> expParser
        _ -> parserZero

    failIfRight :: Parsec T.Text () Exp
    failIfRight = string ")" >> unexpected "dangling right paren"

expParser :: Parsec T.Text () Exp
expParser =
  boolParser
    <|> decimalParser
    <|> stringParser
    <|> identifierParser
    <|> parens
      (lambdaParser <|> beginParser <|> ifParser <|> letParser <|> appParser)
  where
    identifierParser :: Parsec T.Text () Exp
    identifierParser = Id <$> variableParser

    boolParser :: Parsec T.Text () Exp
    boolParser = BLiteral <$> bool

    decimalParser :: Parsec T.Text () Exp
    decimalParser = NLiteral . fromIntegral <$> decimal

    stringParser :: Parsec T.Text () Exp
    stringParser = SLiteral <$> stringLiteral

    expListParser :: Parsec T.Text () [Exp]
    expListParser = many $ whiteSpace >> expParser

    beginParser :: Parsec T.Text () Exp
    beginParser = try $ do
      whiteSpace
      ident <- identifier
      whiteSpace
      case ident of
        "begin" -> do
          whiteSpace
          Begin <$> expListParser
        _ -> parserZero

    lambdaParser :: Parsec T.Text () Exp
    lambdaParser = try $ do
      whiteSpace
      ident <- identifier
      whiteSpace
      case ident of
        "lambda" -> do
          whiteSpace
          vars <- parens $ (whiteSpace >> variableParser) `sepBy` whiteSpace
          whiteSpace
          res <- Lambda vars <$> expListParser
          whiteSpace
          return res
        _ -> parserZero

    letParser :: Parsec T.Text () Exp
    letParser = try $ do
      whiteSpace
      ident <- identifier
      whiteSpace
      case ident of
        "let" -> do
          bindings <- parens ((NE.:|) <$> binding <*> many binding)
          whiteSpace
          res <- Let bindings <$> expListParser
          whiteSpace
          return res
        _ -> parserZero
      where
        binding =
          whiteSpace
            >> parens
              ( do
                  whiteSpace
                  var <- variableParser
                  whiteSpace
                  bnd <- expParser
                  whiteSpace
                  return (var, bnd)
              )

    ifParser :: Parsec T.Text () Exp
    ifParser = try $ do
      whiteSpace
      ident <- identifier
      whiteSpace
      case ident of
        "if" -> do
          whiteSpace
          cond <- expParser
          whiteSpace
          thn <- expParser
          whiteSpace
          If cond thn <$> expParser
        _ -> parserZero

    appParser :: Parsec T.Text () Exp
    appParser = do
      whiteSpace
      rator <- expParser
      whiteSpace
      App rator <$> expListParser
