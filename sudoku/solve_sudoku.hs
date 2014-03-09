module Main (
    main
) where

import Sudoku
import Data.Char (isDigit,isSpace)
import System.IO (readFile)
import Control.Exception (catch)
import Control.Applicative((<$>),(<*>))
import System.Environment (getArgs)

cook = foldr (\x a -> replace x : a) []
   where replace x = if isDigit x || isSpace x then x else ' ' 

parseTask :: FilePath -> IO [[Int]]
parseTask name = ( map 
                       ( map read 
                           . filter (
                                     (&&) 
                                     <$> all isDigit
                                     <*> not . null
                                     )
                           . words
                        )
                  . filter (any isDigit) 
                  . lines
                  . cook
                  ) <$> readFile name

main = do args <- getArgs
          case args of
               [name] -> catch (do task <- parseTask name
                                   printSudoku 
                                               $ getSolution
                                               $ getTask task
                                   return () )
                               ((\_ -> putStrLn "could not read file")
                                   :: IOError -> IO ())
               _      -> putStrLn "no task specified"
