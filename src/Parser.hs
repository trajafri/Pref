module Parser
  ( PTree(..)
  , parse
  , treeToExp
  ) where

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
parse :: [Token] -> Maybe [PTree]
parse [] = Just []
parse ts = do
  (tree, rest) <- parseHelper ts
  parse rest >>= (\finalTrees -> return (tree : finalTrees))

-- Parses non-recursive tokens and returns the parse tree along with
-- the remaining tokens
parseHelper :: [Token] -> Maybe (PTree, [Token])
parseHelper (ID v:ts) = Just (Leaf v, ts)
parseHelper (LParen:ts) = do
  (treeList, restTs) <- parseList ts -- Remove '(' and call parseList
  return (Node treeList, restTs)
parseHelper _ = Nothing

-- Parses recursive tokens (assuming that it is already in a list if "exp")
parseList :: [Token] -> Maybe ([PTree], [Token])
parseList [] = Nothing -- List must terminate with a ')'
parseList (RParen:ts) = Just ([], ts)
parseList els -- Case where we either have a '(' or an ID
 = do
  (expTree, rest) <- parseHelper els
  (allTrees, restTks) <- parseList rest
  return (expTree : allTrees, restTks)

treeToExp :: PTree -> Maybe Exp
treeToExp (Leaf v) -- determine what kind of leaf it is
  | (head v == '"' && (head . reverse) v == '"') = Just $ SLiteral v
  | otherwise = Just $ maybe (Id v) NLiteral (readMaybe v)
treeToExp (Node [Leaf "lambda", Node variables, body]) = do
  bodyExp <- treeToExp body
  if (all isLeaf variables)
    then return $ Lambda (map (\(Leaf v) -> v) variables) bodyExp
    else (sequence (map treeToExp variables ++ [Just bodyExp])) >>=
         (\exps -> return $ App (Id "lambda") exps)
treeToExp (Node [Leaf "let", Node bindings, body]) =
  treeToExp body >>=
  (\bodyExp ->
     if (all isPair bindings)
       then do
         bindingVars <- traverse treeToExp $ map (\(Node [v, _]) -> v) bindings
         bindingVal <- traverse treeToExp $ map (\(Node [_, b]) -> b) bindings
         return $
           Let (zipWith (\(Id v) b -> (v, b)) bindingVars bindingVal) bodyExp
       else do
         bindingExps <- treeToExp $ Node bindings
         return $ App (Id "let") $ [bindingExps] ++ [bodyExp])
treeToExp (Node (rator:rands)) = do
  ratorExp <- treeToExp rator
  randExps <- traverse treeToExp rands
  return $ App ratorExp randExps
treeToExp _ = Nothing
