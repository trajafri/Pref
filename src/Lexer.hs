{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

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
parens p = do
  _   <- char '('
  res <- p
  _   <- char ')'
  return res

-- Parsers shamelessly copied from source
number :: Integer -> Parsec T.Text () Char -> Parsec T.Text () Integer
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

stringLiteral :: Parsec T.Text () T.Text
stringLiteral = do
  x <- do
    str <- between (char '"') (char '"' <?> "end of string") (many stringChar)
    return (foldr (maybe id (:)) "" str) <?> "literal string"

  whiteSpace
  return $ T.pack x

stringChar :: Parsec T.Text () (Maybe Char)
stringChar = (Just <$> stringLetter) <|> stringEscape <?> "string character"

stringLetter :: Parsec T.Text () Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape :: Parsec T.Text () (Maybe Char)
stringEscape = do
  _ <- char '\\'
  do
      _ <- escapeGap
      return Nothing
    <|> do
          _ <- escapeEmpty
          return Nothing
    <|> Just
    <$> escapeCode

escapeEmpty :: Parsec T.Text () Char
escapeEmpty = char '&'

escapeGap :: Parsec T.Text () Char
escapeGap = do
  _ <- many1 space
  char '\\' <?> "end of string gap"

escapeCode :: Parsec T.Text () Char
escapeCode =
  charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

charControl :: Parsec T.Text () Char
charControl = do
  _    <- char '^'
  code <- upper
  return (toEnum (fromEnum code - fromEnum 'A' + 1))

charNum :: Parsec T.Text () Char
charNum = do
  code <-
    decimal
    <|> do
          _ <- char 'o'
          number 8 octDigit
    <|> do
          _ <- char 'x'
          number 16 hexDigit
  if code > 0x10FFFF
    then fail "invalid escape sequence"
    else return (toEnum (fromInteger code))

charEsc :: Parsec T.Text () Char
charEsc = choice (map parseEsc escMap)
 where
  parseEsc (c, code) = do
    _ <- char c
    return code

charAscii :: Parsec T.Text () Char
charAscii = choice (map parseAscii asciiMap)
 where
  parseAscii (asc, code) = try
    (do
      _ <- string asc
      return code
    )


-- escape code tables
escMap :: [(Char, Char)]
escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

asciiMap :: [(String, Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) $ ascii3 ++ ascii2

ascii2codes :: [String]
ascii2codes =
  [ "BS"
  , "HT"
  , "LF"
  , "VT"
  , "FF"
  , "CR"
  , "SO"
  , "SI"
  , "EM"
  , "FS"
  , "GS"
  , "RS"
  , "US"
  , "SP"
  ]

ascii3codes :: [String]
ascii3codes =
  [ "NUL"
  , "SOH"
  , "STX"
  , "ETX"
  , "EOT"
  , "ENQ"
  , "ACK"
  , "BEL"
  , "DLE"
  , "DC1"
  , "DC2"
  , "DC3"
  , "DC4"
  , "NAK"
  , "SYN"
  , "ETB"
  , "CAN"
  , "SUB"
  , "ESC"
  , "DEL"
  ]

ascii2 :: String
ascii2 =
  [ '\BS'
  , '\HT'
  , '\LF'
  , '\VT'
  , '\FF'
  , '\CR'
  , '\SO'
  , '\SI'
  , '\EM'
  , '\FS'
  , '\GS'
  , '\RS'
  , '\US'
  , '\SP'
  ]

ascii3 :: String
ascii3 =
  [ '\NUL'
  , '\SOH'
  , '\STX'
  , '\ETX'
  , '\EOT'
  , '\ENQ'
  , '\ACK'
  , '\BEL'
  , '\DLE'
  , '\DC1'
  , '\DC2'
  , '\DC3'
  , '\DC4'
  , '\NAK'
  , '\SYN'
  , '\ETB'
  , '\CAN'
  , '\SUB'
  , '\ESC'
  , '\DEL'
  ]

decimal :: Parsec T.Text () Integer
decimal = try $ do
  digits <- many1 (satisfy isDigit <?> "digit")
  let n = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

whiteSpace :: Parsec T.Text () ()
whiteSpace =
  skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")

simpleSpace :: Parsec T.Text () ()
simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment :: Parsec T.Text () ()
oneLineComment = do
  _ <- try $ char ';'
  skipMany (satisfy (/= '\n'))
  return ()

multiLineComment :: Parsec T.Text () ()
multiLineComment = do
  _ <- try $ string "#|"
  inComment

inComment :: Parsec T.Text () ()
inComment = inCommentSingle

inCommentSingle :: Parsec T.Text () ()
inCommentSingle =
  do
      _ <- try $ string "|#"
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
