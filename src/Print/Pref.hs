module Print.Pref where

import           Pref
import           Transform.CPS
import           Text.PrettyPrint
import           Syntax.Exp
import           System.IO

indent :: Num a => a
indent = 2

expToDoc :: Exp -> Doc
expToDoc (Id       s) = text s
expToDoc (NLiteral n) = text $ show n
expToDoc (SLiteral s) = text s
expToDoc (Lambda v b) =
  let varDoc = parens . hsep $ text <$> v
  in  parens $ (text "lambda" <+> varDoc) $+$ (nest (pred indent) $ expToDoc b)
expToDoc (If cnd thn els) =
  let cndDoc = expToDoc cnd
      thnDoc = expToDoc thn
      elsDoc = expToDoc els
  in  parens $ (text "if" <+> (cndDoc $+$ thnDoc $+$ elsDoc))
expToDoc (Let bindings b) =
  let (vars, binds) = unzip bindings
      bindDoc       = foldr1 ($+$) $ map parens $ zipWith (<+>)
                                                          (map text vars)
                                                          (map expToDoc binds)
  in  parens $ (text "let" <+> bindDoc) $+$ (nest (pred indent) $ expToDoc b)
expToDoc (App rator rands) =
  parens $ (foldr1 (<+>) $ map expToDoc (rator : rands))
expToDoc (Def v bind) =
  parens $ (text "define" <+> text v) $+$ (nest (pred indent) $ expToDoc bind)


main :: IO ()
main = do
  print "Enter a file path: "
  file <- getLine
  withFile
    file
    ReadMode
    (\h -> do
      fileContent <- hGetContents h
      let (Right ast) = (codeToAst fileContent)
      let finalDoc    = foldr1 ($+$) $ map (expToDoc . cpser) ast
      writeFile "test.pref"
        $ renderStyle (Style PageMode 100 (pred indent)) finalDoc
    )




