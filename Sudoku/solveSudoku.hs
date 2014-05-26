module Main (
    main
) where

import Sudoku.Sudoku
import Sudoku.TaskReader

import Control.Exception (catch)
import System.Environment (getArgs)

main ::  IO ()
main = do args <- getArgs
          case args of
               [name] -> catch (do task <- readTask name
                                   printSudoku
                                               $ getSolution
                                               $ task
                                   return () )
                               ((\_ -> putStrLn "could not read file")
                                   :: IOError -> IO ())
               _      -> putStrLn "no task specified"
