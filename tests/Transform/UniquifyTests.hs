{-# LANGUAGE OverloadedStrings #-}

module Transform.UniquifyTests
  ( uniquifyTest
  )
where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Map                      as M
import qualified Data.Text                     as T
import           Pref
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.Golden.Advanced
import           Transform.Uniquify
import           System.FilePath                ( takeBaseName
                                                , replaceExtension
                                                )

location :: String
location = "tests/Transform/uniquify-tests"

errorMsg :: T.Text
errorMsg = " uniquified incorrectly"

uniquifyTest :: IO TestTree
uniquifyTest = do
  testFiles <- findByExtension [".in"] location
  return $ testGroup
    "Uniquify Tests"
    [ goldenTest
        (takeBaseName f)
        (readFile . flip replaceExtension ".out" $ f)
        (readFile f)
        (\expO input -> either
          (return . Just)
          (const . return $ Nothing)
          (do
            inAst <- leftToString . codeToAst . T.pack $ input
            let uniqued =
                  flip runReader M.empty
                    . flip evalStateT M.empty
                    $ uniquifyProgram inAst M.empty
            expAst <- leftToString . codeToAst . T.pack $ expO
            when
              (uniqued /= expAst)
              (  Left
              $  "equality failed for:\n"
              <> input
              <> "\nExpected:\n"
              <> expO
              <> "\nGot:\n"
              <> (show uniqued)
              )
          )
        )
        (const . return $ ())
    | f <- testFiles
    ]
 where
  leftToString (Left  pe) = Left . show $ pe
  leftToString (Right r ) = Right r
