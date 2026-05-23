{-# LANGUAGE OverloadedStrings #-}

module InterpTests
  ( interpTestList,
  )
where

import Control.Monad
import qualified Data.Text as T hiding
  ( zip,
  )
import Pref
import System.FilePath
  ( replaceExtension,
    takeBaseName,
  )
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced

location :: String
location = "tests/test-files"

errorMsg :: T.Text
errorMsg = " interpreted incorrectly"

interpTestList :: IO TestTree
interpTestList = do
  testFiles <- findByExtension [".pref"] location
  return $
    testGroup "Interpreter Tests" $
      [ goldenTest
          (takeBaseName f)
          (readFile . flip replaceExtension ".out" $ f)
          (readFile f)
          ( \expO input ->
              either
                (return . Just)
                (const . return $ Nothing)
                ( do
                    inVal <- leftsToString . codeToVal . T.pack $ input
                    expVal <- leftsToString . codeToVal . T.pack $ expO
                    when
                      (inVal /= expVal)
                      ( Left $
                          input
                            <> (T.unpack errorMsg)
                            <> "\nExpected:\n"
                            <> (show expVal)
                            <> "\nGot:\n"
                            <> (show inVal)
                      )
                )
          )
          (const . return $ ())
        | f <- testFiles
      ]
  where
    leftsToString (Left eerr) = Left . show $ eerr
    leftsToString (Right (Left pe)) = Left . show $ pe
    leftsToString (Right (Right v)) = Right . show $ v
