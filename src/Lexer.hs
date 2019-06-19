module Lexer
  ( tokenize
  ) where

import Data.List
import Tokens

tokenize :: String -> [Token]
tokenize ls = concatMap (`lexer_acc` "") (words ls)
  where
    lexer_acc [] "" = []
    lexer_acc ('(':ts) "" = LParen : lexer_acc ts ""
    lexer_acc ('(':ts) w = ID (reverse w) : LParen : lexer_acc ts ""
    lexer_acc (')':ts) "" = RParen : lexer_acc ts ""
    lexer_acc (')':ts) w = ID (reverse w) : RParen : lexer_acc ts ""
    lexer_acc (c:ts) w = lexer_acc ts (c : w)
    lexer_acc [] w = [ID (reverse w)]
