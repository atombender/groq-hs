module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Text.Megaparsec (errorBundlePretty)

import qualified Groq.Parser as Parser

main :: IO ()
main = do
  putStrLn "GROQ Parser REPL"
  repl

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  line <- TIO.getLine
  if T.null line
    then return ()
    else do
      case Parser.parseGroq line of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> print ast
      repl
