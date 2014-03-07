module Main (
    main
) where

import Sudoku
import Data.Char (isDigit,isSpace)
import System.IO (readFile)
import Control.Exception (catch)
import Control.Applicative((<$>),(<*>))
import System.Environment (getArgs)

parseTask :: FilePath -> IO [[Int]]
parseTask name = fmap 
                      ( map 
                            ( map read 
                                . filter (
                                          (&&) 
                                          <$> all isDigit
                                          <*> not . null
                                          )
                            )
                       . map words
                       . filter (not . null) 
                       . lines
                       . filter (
                                 (||) 
                                 <$> isDigit
                                 <*> isSpace
                                )
                       )
                      $ readFile name

main = do args <- getArgs
          case args of
               [name] -> do catch (do task <- parseTask name
                                      printSudoku 
                                                  $ getSolution
                                                  $ getTask task
                                      return () )
                                  ((\_ -> putStrLn "could not read file")
                                      :: IOError -> IO ())
               _      -> putStrLn "no task specified"
