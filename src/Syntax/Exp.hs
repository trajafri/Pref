module Syntax.Exp
  ( Identifier (..),
    Exp (..),
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Prettyprinter

newtype Identifier = Var T.Text deriving (Eq, Show, Ord)

instance Pretty Identifier where
  pretty (Var t) = pretty t

data Exp
  = NLiteral Int
  | SLiteral T.Text
  | BLiteral Bool
  | Id Identifier
  | -- ideally, the cases below shouldn't be "special"
    -- better if we have a "syntax definition" that can cover all of these
    Lambda
      [Identifier]
      [Exp]
  | Begin [Exp]
  | If
      Exp
      Exp
      Exp
  | Let
      (NE.NonEmpty (Identifier, Exp))
      [Exp]
  | App
      Exp
      [Exp]
  | Def
      Identifier
      Exp
  deriving (Eq)

indentC :: (Num a) => a
indentC = 1

instance Pretty Exp where
  pretty (Id s) = pretty s
  pretty (NLiteral n) = pretty $ show n
  pretty (SLiteral s) = pretty s
  pretty (BLiteral b) = pretty b
  pretty (Lambda v b) =
    let varDoc = parens . hsep $ pretty <$> v
     in parens $
          hang indentC $
            vcat
              [pretty "lambda" <+> varDoc, pretty b]
  pretty (Begin es) =
    parens $ hang (1 + indentC) $ vcat (pretty "begin" : map pretty es)
  pretty (If cnd thn els) =
    let cndDoc = pretty cnd
        thnDoc = pretty thn
        elsDoc = pretty els
     in parens $ hang (1 + indentC) $ vcat [pretty "if" <+> cndDoc, thnDoc, elsDoc]
  pretty (Let bindings b) =
    let (vars, binds) = unzip . NE.toList $ bindings
        bindDoc =
          vcat $
            map parens $
              zipWith
                (<+>)
                (map pretty vars)
                (map pretty binds)
     in parens $ hang indentC $ vcat [pretty "let" <+> bindDoc, pretty b]
  pretty (App rator rands) = parens (foldr1 (<+>) $ map pretty (rator : rands))
  pretty (Def v bind) =
    parens $
      hang indentC $
        vcat
          [pretty "define" <+> pretty v, pretty bind]

instance Show Exp where
  show = show . pretty
