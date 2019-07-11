module Syntax.Exp
  ( Exp(..)
  )
where

import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc

data Exp
  = Id T.Text
  | NLiteral Int
  | SLiteral T.Text
  | Lambda [T.Text]
           Exp
  | If Exp
       Exp
       Exp
  | Let [(T.Text, Exp)]
        Exp
  | App Exp
        [Exp]
  | Def T.Text
        Exp
  deriving (Eq)

indentC :: Num a => a
indentC = 2

instance Pretty Exp where

  pretty (Id       s) = pretty s
  pretty (NLiteral n) = pretty $ show n
  pretty (SLiteral s) = pretty s
  pretty (Lambda v b) =
    let varDoc = parens . hsep $ pretty <$> v
    in  parens
          $ vcat [pretty "lambda" <+> varDoc, nest (pred indentC) $ pretty b]
  pretty (If cnd thn els) =
    let cndDoc = pretty cnd
        thnDoc = pretty thn
        elsDoc = pretty els
    in  parens $ (pretty "if" <+> (vcat [cndDoc, thnDoc, elsDoc]))
  pretty (Let bindings b) =
    let (vars, binds) = unzip bindings
        bindDoc       = vcat $ map parens $ zipWith (<+>)
                                                    (map pretty vars)
                                                    (map pretty binds)
    in  parens $ vcat [pretty "let" <+> bindDoc, nest (pred indentC) $ pretty b]
  pretty (App rator rands) =
    parens $ (foldr1 (<+>) $ map pretty (rator : rands))
  pretty (Def v bind) = parens
    $ vcat [pretty "define" <+> pretty v, nest (pred indentC) $ pretty bind]

instance Show Exp where
  show = show . pretty

