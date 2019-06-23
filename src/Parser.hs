module Parser
  ( PTree(..)
  , parse
  , treeToExp
  ) where

import Errors
import Exp
import Lexer
import Text.Read
import Tokens

data PTree
  = Leaf String
  | Node [PTree]
  deriving (Eq)

instance Show PTree where
  show (Leaf v) = v
  show (Node ls) = ('(' : (unwords . map show) ls) ++ ")"

isLeaf :: PTree -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

isPair :: PTree -> Bool
isPair (Node [Leaf _, Leaf _]) = True
isPair _ = False

{-- Idea:
  To convert a series of Tokens to a Parse Tree, we will assume
  that a '(' indicates a list of sub parse tree, that is terminated by ')'.
  So, we will have a mutually recursive recursive function that handles ID's and
  list of parse trees separately.

  TODO: Replace Maybe with a monad that contains error msg instead of Nothing
--}
parse :: [Token] -> Either Error [PTree]
parse [] = return []
parse ts = do
  (tree, rest) <- parseHelper ts
  parse rest >>= (\finalTrees -> return (tree : finalTrees))

-- Parses non-recursive tokens and returns the parse tree along with
-- the remaining tokens
parseHelper :: [Token] -> Either Error (PTree, [Token])
parseHelper (ID v:ts) = return (Leaf v, ts)
parseHelper (LParen:ts) = do
  (treeList, restTs) <- parseList ts -- Remove '(' and call parseList
  return (Node treeList, restTs)
parseHelper rp =
  Left $
  ParseError $
  "Found an expression beginning with a right parenthesis:\n" ++ show rp

-- Parses recursive tokens (assuming that it is already in a list if "exp")
parseList :: [Token] -> Either Error ([PTree], [Token])
parseList [] = Left $ ParseError "A left parenthesis was not terminated" -- List must terminate with a ')'
parseList (RParen:ts) = return ([], ts)
parseList els -- Case where we either have a '(' or an ID
 = do
  (expTree, rest) <- parseHelper els
  (allTrees, restTks) <- parseList rest
  return (expTree : allTrees, restTks)

-- Currently, I don't like how I have to parse out let's, if's etc.
-- Maybe there is a better way.
treeToExp :: PTree -> Either Error Exp
treeToExp (Leaf v) -- determine what kind of leaf it is
  | head v == '\"' && last v == '\"' -- String leaf
   = return $ SLiteral (zipWith const (tail v) (tail . tail $ v))
  | otherwise = return $ maybe (Id v) NLiteral (readMaybe v)
treeToExp (Node [Leaf "lambda", Node variables, body]) = do
  bodyExp <- treeToExp body
  if all isLeaf variables
    then return $ Lambda (map (\(Leaf v) -> v) variables) bodyExp
    else App (Id "lambda") <$>
         sequence (map treeToExp variables ++ [return bodyExp])
treeToExp (Node [Leaf "let", Node bindings, body]) =
  treeToExp body >>=
  (\bodyExp ->
     if all isPair bindings
       then do
         bindingVars <- traverse (treeToExp . \(Node [v, _]) -> v) bindings
         bindingVal <- traverse (treeToExp . \(Node [_, b]) -> b) bindings
         return $
           Let (zipWith (\(Id v) b -> (v, b)) bindingVars bindingVal) bodyExp
       else do
         bindingExps <- treeToExp $ Node bindings
         return $ App (Id "let") $ bindingExps : [bodyExp])
treeToExp (Node [Leaf "if", cond, thn, els]) = do
  condExp <- treeToExp cond
  thenExp <- treeToExp thn
  elseExp <- treeToExp els
  return $ If condExp thenExp elseExp
treeToExp (Node [Leaf "define", Leaf id, binding]) = do
  bindingExp <- treeToExp binding
  return $ Def id bindingExp
treeToExp (Node (rator:rands)) = do
  ratorExp <- treeToExp rator
  randExps <- traverse treeToExp rands
  return $ App ratorExp randExps
treeToExp _ =
  Left $
  ParseError "Found (). What does this mean?\nDid you mean to make a thunk?" -- I think this should only happen in case of "()"
