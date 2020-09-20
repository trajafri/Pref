{-# LANGUAGE OverloadedStrings, ViewPatterns  #-}

module Lexer
  ( tokenize
  )
where

import           Data.Char
import qualified Data.Text                     as T
import           Errors
import           Syntax.Tokens

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
validIdChar c = all ($ c) [not . isSpace, (')' /=), ('(' /=), ('\"' /=)]
