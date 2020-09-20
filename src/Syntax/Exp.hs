module Syntax.Exp
  ( Exp(..)
  )
where

import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc

data Exp
  = Empty
  | Id T.Text
  | NLiteral Int
  | SLiteral T.Text
  | BLiteral Bool
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

  pretty (Id s)       = pretty s
  pretty Empty        = pretty "empty"
  pretty (NLiteral n) = pretty $ show n
  pretty (SLiteral s) = pretty s
  pretty (BLiteral b) = pretty b
  pretty (Lambda v b) =
    let varDoc = parens . hsep $ pretty <$> v
    in  parens $ hang (pred indentC) $ vcat
          [pretty "lambda" <+> varDoc, pretty b]
  pretty (If cnd thn els) =
    let cndDoc = pretty cnd
        thnDoc = pretty thn
        elsDoc = pretty els
    in  parens $ hang indentC $ vcat [pretty "if" <+> cndDoc, thnDoc, elsDoc]
  pretty (Let bindings b) =
    let (vars, binds) = unzip bindings
        bindDoc       = vcat $ map parens $ zipWith (<+>)
                                                    (map pretty vars)
                                                    (map pretty binds)
    in  parens $ hang (pred indentC) $ vcat [pretty "let" <+> bindDoc, pretty b]
  pretty (App rator rands) = parens (foldr1 (<+>) $ map pretty (rator : rands))
  pretty (Def v     bind ) = parens $ hang (pred indentC) $ vcat
    [pretty "define" <+> pretty v, pretty bind]

instance Show Exp where
  show = show . pretty

