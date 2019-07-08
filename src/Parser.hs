{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( PTree(..)
  , parse
  , treeToExp
  , prefParser
  )
where

import           Control.Monad.Except
import qualified Data.Text                     as T
import           Errors
import           Lexer
import           Syntax.Exp
import           Prelude                 hiding ( id )
import           Text.Read
import           Syntax.Tokens
import           Text.Parsec             hiding ( parse )

prefParser :: Parsec String () Exp
prefParser = defineParser <|> expParser
 where
  defineParser :: Parsec String () Exp
  defineParser = try $ Lexer.parens $ do
    define <- identifier
    case define of
      "define" -> whiteSpace >> expParser
      _        -> mzero

expParser :: Parsec String () Exp
expParser =
  idParser
    <|> decimalParser
    <|> stringParser
    <|> (Lexer.parens (lambdaParser <|> letParser <|> ifParser <|> appParser))

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
        vars <- Lexer.parens $ many identifier
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
        bindings <- Lexer.parens $ many
          ( Lexer.parens
          $ (do
              ident <- identifier
              whiteSpace
              exp <- expParser
              return (T.pack ident, exp)
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

data PTree
  = Leaf T.Text
  | Node [PTree]
  deriving (Eq)

instance Show PTree where
  show (Leaf v ) = T.unpack v
  show (Node ls) = ('(' : (unwords . map show) ls) <> ")"

isLeaf :: PTree -> Bool
isLeaf (Leaf _) = True
isLeaf _        = False

isPair :: PTree -> Bool
isPair (Node [Leaf _, _]) = True
isPair _                  = False

{-- Idea:
  To convert a series of Tokens to a Parse Tree, we will assume
  that a '(' indicates a list of sub parse tree, that is terminated by ')'.
  So, we will have a mutually recursive recursive function that handles ID's and
  list of parse trees separately.
--}
parse :: [Token] -> Either Error [PTree]
parse [] = return []
parse ts = do
  (tree, rest) <- parseHelper ts
  finalTrees   <- parse rest
  return (tree : finalTrees)

-- Parses non-recursive tokens and returns the parse tree along with
-- the remaining tokens
parseHelper :: [Token] -> Either Error (PTree, [Token])
parseHelper (ID v   : ts) = return (Leaf v, ts)
parseHelper (LParen : ts) = do
  (treeList, restTs) <- parseList ts -- Remove '(' and call parseList
  return (Node treeList, restTs)
parseHelper rp =
  Left
    $  ParseError
    $  "Found an expression beginning with a right parenthesis:\n"
    <> (T.pack . show $ rp)

-- Parses recursive tokens (assuming that it is already in a list if "exp")
parseList :: [Token] -> Either Error ([PTree], [Token])
parseList [] = Left $ ParseError "A left parenthesis was not terminated" -- List must terminate with a ')'
parseList (RParen : ts) = return ([], ts)
parseList els = do
  (expTree , rest   ) <- parseHelper els
  (allTrees, restTks) <- parseList rest
  return (expTree : allTrees, restTks) -- Case where we either have a '(' or an ID

-- Currently, I don't like how I have to parse out let's, if's etc.
-- Maybe there is a better way.
treeToExp :: PTree -> Either Error Exp
treeToExp (Leaf v)
  | -- determine what kind of leaf it is
    T.head v == '\"' && T.last v == '\"' -- String leaf
                                         = return
  $ SLiteral (T.init . T.tail $ v)
  | otherwise = return $ maybe (Id v) NLiteral (readMaybe . T.unpack $ v)
treeToExp (Node [Leaf "lambda", Node variables, body]) = do
  bodyExp <- treeToExp body
  if all isLeaf variables
    then return $ Lambda (map (\(Leaf v) -> v) variables) bodyExp
    else App (Id "lambda")
      <$> sequence (map treeToExp variables <> [return bodyExp])
treeToExp (Node [Leaf "let", Node bindings, body]) = do
  bodyExp <- treeToExp body
  if all isPair bindings
    then do
      bindingVars <- traverse (treeToExp . \(Node [v, _]) -> v) bindings
      bindingVal  <- traverse (treeToExp . \(Node [_, b]) -> b) bindings
      return
        $ Let (zipWith (\(Id v) b -> (v, b)) bindingVars bindingVal) bodyExp
    else do
      bindingExps <- treeToExp $ Node bindings
      return $ App (Id "let") $ bindingExps : [bodyExp]
treeToExp (Node [Leaf "if", cond, thn, els]) = do
  condExp <- treeToExp cond
  thenExp <- treeToExp thn
  elseExp <- treeToExp els
  return $ If condExp thenExp elseExp
treeToExp (Node [Leaf "define", Leaf id, binding]) = do
  bindingExp <- treeToExp binding
  return $ Def id bindingExp
treeToExp (Node (rator : rands)) = do
  ratorExp <- treeToExp rator
  randExps <- traverse treeToExp rands
  return $ App ratorExp randExps
treeToExp _ = Left
  $ ParseError "Found (). What does this mean?\nDid you mean to make a thunk?" -- I think this should only happen in case of "()"
