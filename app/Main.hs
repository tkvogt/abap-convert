module Main where

import System.Environment(getArgs)
--import System.IO(readFile, putStrLn)
import ABAP(abapModernSyntax)
import qualified Data.Text as T
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do args <- getArgs
          check args

check :: [String] -> IO ()
check ["-i",filePath] = do
            content <- readFile filePath
            putStr (T.unpack (either T.pack id (pretty (abapModernSyntax filePath (T.pack content)))))
  where pretty (Left l) = Left (errorBundlePretty l)
        pretty (Right r) = Right r

check _ = putStrLn "Usage: abap-convert -i <abap-file>"
