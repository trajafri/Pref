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

parse :: Parsec String () [Exp]
parse = many $ defineParser <|> expParser
 where
  defineParser :: Parsec String () Exp
  defineParser = try $ parens $ do
    define <- identifier
    case define of
      "define" -> whiteSpace >> expParser
      _        -> mzero

expParser :: Parsec String () Exp
expParser =
  idParser
    <|> decimalParser
    <|> stringParser
    <|> (parens (lambdaParser <|> letParser <|> ifParser <|> appParser))

 where
  -- Since I made identifier, I use an explicit try here.
  idParser :: Parsec String () Exp
  idParser = try identifier >>= (return . Id . T.pack)

  decimalParser :: Parsec String () Exp
  decimalParser = decimal >>= (return . NLiteral . fromIntegral)

  stringParser :: Parsec String () Exp
  stringParser = stringLiteral >>= (return . SLiteral . T.pack)

  lambdaParser :: Parsec String () Exp
  lambdaParser = try $ do
    ident <- identifier
    whiteSpace
    case ident of
      "lambda" -> do
        vars <- parens $ many identifier
        whiteSpace
        body <- expParser
        return (Lambda (map T.pack vars) body)
      _ -> mzero

  letParser :: Parsec String () Exp
  letParser = try $ do
    ident <- identifier
    whiteSpace
    case ident of
      "let" -> do
        bindings <- parens $ many
          ( parens
          $ (do
              var <- identifier
              whiteSpace
              binding <- expParser
              return (T.pack var, binding)
            )
          )
        whiteSpace
        body <- expParser
        return (Let bindings body)
      _ -> mzero

  ifParser :: Parsec String () Exp
  ifParser = try $ do
    ident <- identifier
    whiteSpace
    case ident of
      "if" -> do
        whiteSpace
        cond <- expParser
        whiteSpace
        thn <- expParser
        whiteSpace
        els <- expParser
        return (If cond thn els)
      _ -> mzero

  appParser :: Parsec String () Exp
  appParser = do
    rator <- expParser
    whiteSpace
    rands <- many $ whiteSpace >> expParser
    return (App rator rands)
