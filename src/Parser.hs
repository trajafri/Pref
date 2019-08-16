{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parse
  )
where

import           Control.Monad.Except
import qualified Data.Text                     as T
import           Lexer
import           Syntax.Exp
import           Prelude                 hiding ( id )
import           Text.Parsec             hiding ( parse )

parse :: Parsec T.Text () [Exp]
parse = many $ whiteSpace >> (defineParser <|> expParser <|> failIfRight)
 where
  defineParser :: Parsec T.Text () Exp
  defineParser = try . parens $ do
    define <- identifier
    whiteSpace
    case define of
      "define" -> do
        ident <- identifier
        whiteSpace
        Def ident <$> expParser
      _ -> mzero

  failIfRight :: Parsec T.Text () Exp
  failIfRight = string ")" >> unexpected "dangling right paren"

expParser :: Parsec T.Text () Exp
expParser = idParser <|> decimalParser <|> stringParser <|> parens
  (lambdaParser <|> letParser <|> ifParser <|> appParser)

 where
  idParser :: Parsec T.Text () Exp
  idParser = identifier >>= (return . Id)

  decimalParser :: Parsec T.Text () Exp
  decimalParser = decimal >>= (return . NLiteral . fromIntegral)

  stringParser :: Parsec T.Text () Exp
  stringParser = stringLiteral >>= (return . SLiteral)

  lambdaParser :: Parsec T.Text () Exp
  lambdaParser = try $ do
    whiteSpace
    ident <- identifier
    whiteSpace
    case ident of
      "lambda" -> do
        whiteSpace
        vars <- parens $ (whiteSpace >> identifier) `sepBy` whiteSpace
        whiteSpace
        res <- Lambda vars <$> expParser
        whiteSpace
        return res
      _ -> mzero

  letParser :: Parsec T.Text () Exp
  letParser = try $ do
    whiteSpace
    ident <- identifier
    whiteSpace
    case ident of
      "let" -> do
        bindings <- parens $ many $ whiteSpace >> parens
          (do
            whiteSpace
            var <- identifier
            whiteSpace
            binding <- expParser
            whiteSpace
            return (var, binding)
          )
        whiteSpace
        res <- Let bindings <$> expParser
        whiteSpace
        return res
      _ -> mzero

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
      _ -> mzero

  appParser :: Parsec T.Text () Exp
  appParser = do
    whiteSpace
    rator <- expParser
    whiteSpace
    rands <- many $ whiteSpace >> expParser
    return (App rator rands)
