module Sudoku.TaskReader (
       parseTask, readTask
) where

import Sudoku.Sudoku

import Data.Char (isDigit,isSpace)
import System.IO (readFile)
import Control.Applicative((<$>),(<*>))

-- | Replace all non-relevant chars with spaces for the sake of simpler parsing.
cook ::  [Char] -> [Char]
cook = foldr (\x a -> replace x : a) []
   where replace x = if isDigit x || isSpace x then x else ' '

-- | Open file and get its contents as puzzle.
parseInt :: String -> [[Int]]
parseInt = map
                ( map read
                    . filter ( (&&)
                            <$> all isDigit
                            <*> not . null)
                    . words)
          . filter (any isDigit)
          . lines
          . cook

parseTask :: String -> [Cell Int]
parseTask = getTask . parseInt

readTask :: FilePath -> IO [Cell Int]
readTask name = parseTask <$> readFile name
