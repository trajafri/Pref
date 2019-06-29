module Lexer
  ( tokenize
  ) where

import Data.Char
import Data.List
import Errors
import Tokens

-- It might be better to use Text instead of String, but
-- this is just a toy project so no worries
tokenize :: String -> Either Error [Token]
tokenize = lexer
  where
    lexer :: String -> Either Error [Token]
    lexer [] = return []
    lexer ('(':ts) = lexer ts >>= \res -> return $ LParen : res
    lexer (')':ts) = lexer ts >>= \res -> return $ RParen : res
    lexer (c:ts)
      | isDigit c = do
        let (num, rest) = span isDigit ts
        res <- lexer rest
        return $ ID (c : num) : res
      | validIdChar c = do
        let (rId, rest) = span validIdChar ts
        res <- lexer rest
        return $ ID (c : rId) : res
      | c == '\"' --The only reason to make lexing hard :(
       = do
        let (str, rest) = span (/= '\"') ts
        case rest of
          '\"':rrest -> do
            res <- lexer rrest
            return $ ID (c : str ++ [c]) : res
          _ ->
            Left . LexerError $
            "LexerError: A string was not terminated with a quote."
      | otherwise = lexer ts --Unidentified character

validIdChar :: Char -> Bool
validIdChar c = all ($ c) [not . isSpace, (')' /=), ('(' /=), ('\"' /=)]
