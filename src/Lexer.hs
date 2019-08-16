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
import           Data.List                      ( nub )
import qualified Data.Text                     as T
import           Text.Parsec

{- Regarding using the "user state" in ParsecT:
   If we were to stack ParsecT on some monad M, the
   difference would be that ParsecT's state
   (aka "user state") also backtracks on failure, but
   the effects of M do not!
   Imagine backtracking after performing IO. The IO
   effect would should up even if ParsecT backtracked.
-}

identifier :: Parsec T.Text () T.Text
identifier = try $ do
  idName@(idC : ids) <- many1 (alphaNum <|> satisfy validIdChar)
  if isDigit idC && all isDigit ids then mzero else return $ T.pack idName

parens :: Parsec T.Text () a -> Parsec T.Text () a
parens p = try $ do
  _   <- string "("
  res <- p
  _   <- string ")"
  return res

stringLiteral :: Parsec T.Text () T.Text
stringLiteral = try $ do
  _   <- string "\""
  res <- many alphaNum
  _   <- string "\""
  return $ T.pack res

-- Below two shamelessly copied from source
decimal :: Parsec T.Text () Integer
decimal = try $ do
  digits <- many1 $ (satisfy isDigit <?> "digit")
  let n = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

whiteSpace :: Parsec T.Text () ()
whiteSpace =
  skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")

simpleSpace :: Parsec T.Text () ()
simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment :: Parsec T.Text () ()
oneLineComment = do
  _ <- try (string ";")
  skipMany (satisfy (/= '\n'))
  return ()

multiLineComment :: Parsec T.Text () ()
multiLineComment = do
  _ <- try (string "#|")
  inComment

inComment :: Parsec T.Text () ()
inComment = inCommentSingle

inCommentSingle :: Parsec T.Text () ()
inCommentSingle =
  do
      _ <- try (string "|#")
      return ()
    <|> do
          skipMany1 (noneOf startEnd)
          inCommentSingle
    <|> do
          _ <- oneOf startEnd
          inCommentSingle
    <?> "end of comment"
  where startEnd = nub ("|#" ++ "#|")

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
