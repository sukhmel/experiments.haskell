module Main (
    main
) where

import Sudoku
import System.IO (readFile)
import Control.Exception (catch)
import System.Environment (getArgs)

parseTask name = fmap (read :: [Char] -> [[Int]]) $ readFile name

main = do args <- getArgs
          case args of
               [name] -> do catch (do task <- parseTask name
                                      printSudoku $ getSolution $ getTask task
                                      return () )
                                  ((\_ -> putStrLn "could not read file")
                                      :: IOError -> IO ())
               _      -> putStrLn "no task specified"
