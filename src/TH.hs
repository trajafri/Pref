{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module TH where

import           Parser
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Data.Text                     as T
import           Control.Monad
import qualified Syntax.Exp                    as E
import           Text.Parsec                    ( runParser )
import           Text.Parsec.Error

udefQuoter :: String -> String -> Q a
udefQuoter e _ = error e

prefQuoter :: QuasiQuoter
prefQuoter = QuasiQuoter prefExpQ
                         (udefQuoter "no support for quoting patterns")
                         (udefQuoter "no support for quoting types")
                         prefDefQ

prefExpQ :: String -> Q Exp
prefExpQ code = either qError prefExpsToHs $ runParser parse () "" $ T.pack
  code
 where
  qError pe = [e|error $er|] where er = return . LitE . StringL . show $ pe

  prefExpsToHs :: [E.Exp] -> Q Exp
  prefExpsToHs es
    | length es /= 1 = error "expected exactly one pref expression"
    | otherwise      = prefExpToHs . head $ es

prefExpToHs :: E.Exp -> Q Exp
prefExpToHs e = case e of
  E.Empty      -> [|[]|]
  E.Id       t -> prefIdToHs . show $ t
  E.NLiteral i -> return . LitE . IntegerL . toInteger $ i
  E.SLiteral t -> return . LitE . StringL . show $ t
  E.BLiteral b -> return . ConE . mkName $ bName
    where bName = if b then "True" else "False"
  E.Lambda vars b -> LamE hVars <$> prefExpToHs b
    where hVars = map (VarP . mkName . tail . init . show) vars
  E.If q t f -> CondE <$> prefExpToHs q <*> prefExpToHs t <*> prefExpToHs f
  E.Let bs b -> hBinds >>= \hBs -> LetE hBs <$> prefExpToHs b
   where
    hBinds = mapM
      (\(t, tb) -> prefExpToHs tb >>= \hsBody -> return
        $ ValD (VarP . mkName . tail . init . show $ t) (NormalB hsBody) []
      )
      bs
  E.App rator es -> prefExpToHs rator
    >>= \hE -> foldM (\f rand -> AppE f <$> prefExpToHs rand) hE es
  E.Def _ _ -> error "define form not supported"

prefIdToHs :: String -> Q Exp
prefIdToHs e = return $ case e of
  "\"+\""      -> VarE . mkName $ "+"
  "\"-\""      -> VarE . mkName $ "-"
  "\"*\""      -> VarE . mkName $ "*"
  "\"cons\""   -> ConE . mkName $ ":"
  "\"car\""    -> VarE . mkName $ "head"
  "\"cdr\""    -> VarE . mkName $ "tail"
  "\"empty\""  -> ConE . mkName $ "[]"
  "\"empty?\"" -> VarE . mkName $ "null"
  "\"zero?\""  -> AppE (VarE . mkName $ "==") (LitE . IntegerL $ 0)
  _            -> VarE . mkName $ tail . init $ e

prefDefQ :: String -> Q [Dec]
prefDefQ code = either qError expsToHs $ runParser parse () "" $ T.pack code
 where
  qError :: ParseError -> Q [Dec]
  qError pe =
    error ("error while parsing Pref in declaration form:\n" ++ (show pe))


  expsToHs :: [E.Exp] -> Q [Dec]
  expsToHs es | length es /= 1 = error "expected exactly one pref declaration"
              | otherwise      = (flip (:) []) <$> (defToHs $ head es)

  defToHs :: E.Exp -> Q Dec
  defToHs (E.Def n b) = prefExpToHs b >>= \hsBody ->
    return $ ValD (VarP . mkName . tail . init . show $ n) (NormalB hsBody) []
  defToHs _ = error "expects only a definition at the top level"

