{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Either(isRight)
import qualified Data.Text as T
import System.FilePath(takeDirectory, (</>))
import System.Environment(getExecutablePath)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec.Error (errorBundlePretty)

import ABAP(convertToRust)

main = do defaultMain tests

tests :: TestTree
tests = testGroup "Simple Abap files"
      [ testCase file0 $
          do file <- testFile file0
             putStrLn ("file " ++ show (T.length (T.pack file)))
             let conv = pretty (convertToRust file0 (T.pack file))
             (fmap (const "") conv) @?= (Right "") -- we are only interested in the error messages, until the parser is correct
      , testCase file1 $
          do file <- testFile file1
             putStrLn ("file " ++ show (T.length (T.pack file)))
             let conv = pretty (convertToRust file1 (T.pack file))
             (fmap (const "") conv) @?= (Right "")
      ]
  where pretty (Left l) = Left (errorBundlePretty l)
        pretty (Right r) = Right r

file0 = "min.abap"
file1 = "branch.abap"

testFile f =
  do execPath <- getExecutablePath
     let projectPath = goUp 10 execPath
     --putStrLn "projectPath"
     --putStrLn projectPath
     readFile (projectPath </> "test" </> f)

goUp :: Int -> FilePath -> FilePath
goUp 0 path = path
goUp n path = goUp (n-1) (takeDirectory path)
