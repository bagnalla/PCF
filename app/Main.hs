module Main where

import Control.Monad
import System.Environment (getArgs)
import System.IO (hGetContents)
import Ast
import Interp (interpProg)
import Parser (parseProg)
import Preprocessor (importLines, substImports)
import Tycheck (tycheckProg)

main :: IO ()
main = do
  args <- getArgs

  -- Read in source file
  src <- readFile $ case args of
                      [f] -> f
                      []  -> error "Error: no input file"
  -- Locate import commands
  let imports = importLines (lines src)

  -- Map imports to their corresponding source code
  import_srcs <- mapM
    (\(lnum, imps) ->
        sequence (lnum, (mapM (readFile . (++ ".cf")) imps)))
    imports

  -- Replace imports by inlining their source code
  let src' = substImports src import_srcs
  
  -- Parse and typecheck the final source code.
  -- On success, run the interpreter on the AST
  let results = case parseAndTyCheck src' of
        Left s -> error s
        Right p' -> interpProg p'

  -- Get the last result and show it
  let result = results!!(length results - 1)
  case result of
    Left s -> putStrLn $ show s
    Right t -> putStrLn $ show t

  -- Show all results
  -- putStrLn $ show results

parseAndTyCheck f = do
  p  <- parseProg "" f
  p' <- tycheckProg p
  return p'
