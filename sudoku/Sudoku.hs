module Sudoku (
    sudoku, getSolution, printSudoku, updateValues, makeCells, getTask
) where

import Control.Applicative

-- size of smaller part of puzzle
partSize = 3
utf      = False

-- make puzzle out of array of values
getTask task = updateValues sudoku $ makeCells task

-- unused: get all combinations where one element is taken from each input list 
permutation :: (Eq a) => [[a]] -> [[a]]
permutation [] = [[]]
permutation (x:xs) = [ a : b | a <- x, b <- permutation xs, a `notElem` b ]

-- Suppose allright x y, and check if it is possible to create combination of 
-- elements from y (one element from each list) containing all elements from x.
allright :: (Eq a) => [a] -> [[a]] -> Bool
allright [] _     = True
allright _ []     = False
allright (x:xs) y = any (allright xs) (branch x y)

-- Create list of possible continuations
branch :: (Eq a) => a -> [[a]] -> [[[a]]]
branch x ys = [ filter (not . null) $ c ++ rest | c <- containing ]
          where rest       = filter (notElem x) ys
                containing = exclusiveSplit . map clean . filter (elem x) $ ys
                clean      = filter (/= x)

-- Split list into set of list containing all but one elements
exclusiveSplit :: [a] -> [[a]]
exclusiveSplit = init . splitter
                 where splitter []     = [[]]
                       splitter (x:xs) = xs : map (x:) (splitter xs)

-- Cell of a puzzle
data Cell a = Cell {values :: [a], column :: Int, row :: Int}

instance Eq (Cell a) where
    (Cell _ lx ly) == (Cell _ rx ry) = (lx == rx) && (ly == ry)

-- Positional constraint
part :: Cell a -> Int
part (Cell _ r c) = partSize * (r `div` partSize) + (c `div` partSize)

-- Fancy formatting
romans = [(1000, "M") ,( 900,"CM") ,( 500, "D") ,( 400,"CD") ,( 100, "C")
         ,(  90,"XC") ,(  50, "L") ,(  40,"XL") ,(  10, "X") ,(   9,"IX")
         ,(   5, "V") ,(   4,"IV") ,(   1, "I") ]

roman :: Int -> String
roman 0 = ""
roman n = snd r ++ roman (n - fst r)
          where r = head $ filter ((n >=) . fst) romans

instance (Show a) => Show (Cell a) where
    show c = show (values c) ++ " @ "
          ++ show (row c) ++ " x "
          ++ show (column c) ++ " - "
          ++ roman(1 + part c)

-- empty puzzle
sudoku = [Cell [1..partSize^2] x y
            | x <- [0..(partSize^2-1)]
            , y <- [0..(partSize^2-1)]
         ]

-- print sudoku on screen
printSudoku s = mapM_ print $ showSudoku s

-- prepare string representation to be printed
showSudoku s = [concat [v (get s $ real x y) x y | x<- indices] | y <- indices]
           where indices = [0..partSize^2 + partSize]
                 v c x y | vertical || horizontal = border (vertical,
                                                            horizontal,
                                                            left,
                                                            right,
                                                            top,
                                                            bottom)
                         | otherwise = case length vars of
                                          1 -> ' ' : show (head vars) ++ " "
                                          0 -> " x "
                                          _ -> " . "
                       where vertical   = x `mod` (partSize + 1) == 0
                             horizontal = y `mod` (partSize + 1) == 0
                             top        = y == head indices
                             bottom     = y == last indices
                             left       = x == head indices
                             right      = x == last indices
                             vars       = values $ head c
                 real x y = (x - (1 + x `div` (partSize + 1)),
                             y - (1 + y `div` (partSize + 1)))

                 border decision@(vert, horiz, left, right, top, bottom) =
                          case decision of
                               (True, False, _, _, _, _)      -> " | " /-> " │ "
                               (True, True, True, _, True, _) -> " +-" /-> " ┌─"
                               (True, True, True, _, _, True) -> " +-" /-> " └─"
                               (True, True, _, True, True, _) -> "-+ " /-> "─┐ "
                               (True, True, _, True, _, True) -> "-+ " /-> "─┘ "
                               (True, True, _, _, _, True)    -> "-+-" /-> "─┴─"
                               (True, True, _, _, True, _)    -> "-+-" /-> "─┬─"
                               (True, True, True, _, _, _)    -> " +-" /-> " ├─"
                               (True, True, _, True, _, _)    -> "-+ " /-> "─┤ "
                               (True, True, _, _, _, _)       -> "-+-" /-> "─┼─"
                               (_, True, _, _, _, _)          -> "---" /-> "───"
                 a /-> b      = if not utf then a else b
                 get s (x, y) = filter 
                                ((&&) <$> (x ==) . column <*> (y ==) . row) s


updateCell :: [Cell Int] -> Cell Int -> Cell Int
updateCell list cell@(Cell v x y) = Cell v' x y
        where v' = if length v < 2 then filter (not . same) v
                                   else filter correct v
              correct  c = all (good c) [row, column, part]
              same c = c `elem` (concat . concatMap chosen
                                 $ [row, column, part])
              good a thing  = allright required vars
                          where vars = [a] : callBy thing
              chosen thing  = filter ((==1) . length) $ callBy thing
              necessary f   = map values $ filter (unique f) list
              unique        = ((&&) <$> (/=cell) <*>)
              sameBy f      = ( == f cell) . f
              callBy f      = necessary $ sameBy f
              required      = [1..partSize^2]

updateValue :: [Cell Int] -> Cell Int -> [Cell Int]
updateValue list cell@(Cell v x y) = [f c | c <- list]
                                 where f c | c /= cell = c
                                           | c == cell = cell

decide (Cell [] _ _)     = []
decide (Cell (v:vs) x y) = Cell [v] x y : decide (Cell vs x y)

updateValues list []     = list
updateValues list (c:cs) = updateValue (updateValues list cs)  c

makeCells list = [ a | Just a <- concat $ zipWith apply (map cook list) indices]
             where cook list = zipWith make list indices
                   values  = [1..partSize^2]
                   indices = map (\x->x-1) values
                   make value x y =  if  value `elem` values
                                         then Just $ Cell [value] x y
                                         else Nothing

upgrade list = if map values list' /= map values list then upgrade list'
                                                      else list'
           where list' = [updateCell list cell | cell <- list]

solve list = if (==0) $ length . filter null . map values $ list
                then if not . null $ undecided
                        then filter (\y -> not . any null . map values $ y) 
                             possibilities
                        else [list']
                else [[Cell [] 0 0]]
         where undecided     = filter ((1<) . length . values) list'
               decision      = concatMap decide undecided
               list'         = upgrade list
               possibilities = concatMap (\x -> solve $ 
                                                   updateValues list' [x]) 
                                             decision

getSolution = head . solve

apply :: [a -> b] -> a -> [b]
apply [] _     = []
apply (f:fs) a = f a : apply fs a
