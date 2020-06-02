{-# LANGUAGE OverloadedStrings #-}

module Transform.UniquifyTests
  ( uniquifyTest
  )
where

import           Control.Monad
import           Control.Monad.State
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
        (\input output -> either
          (return . Just)
          (const . return $ Nothing)
          (do
            inAst <- leftToString . codeToAst . T.pack $ input
            let uniqued = flip evalState M.empty $ uniquifyProgram inAst
            outAst <- leftToString . codeToAst . T.pack $ output
            when (uniqued /= outAst)
                 (Left $ "equality failed for " <> input <> " " <> output)
          )
        )
        (const . return $ ())
    | f <- testFiles
    ]
 where
  leftToString (Left  pe) = Left . show $ pe
  leftToString (Right r ) = Right r
