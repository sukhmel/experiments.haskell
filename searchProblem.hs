{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Main where

import Data.List ((\\))

type Choice m s   = ([m], s)
type Space m s    = [Choice m s]
type Possible m s = [(m, s)]
type Strategy m s = Space m s -> Space m s -> Space m s

class SearchProblem m s where
    translate    :: s -> Possible m s

    space        :: s -> Space m s
    space s      = ones ++ expand
             where ones   = [([m], s') | (m, s') <- step]
                   step   = translate s
                   expand = [(m:ms, t) | (m, s') <- step
                                       , (ms, t) <- space s']

    spaceBy      :: Strategy m s -> s -> Space m s
    spaceBy f b  = expand $ step ([], b)
             where expand []     = []
                   expand (t:ts) = t : expand (step t `f` ts)
                   step (ms, s)    = [(m:ms, s') | (m, s') <- translate s]

    strategy     :: Strategy m s
    strategy     = depthFirst

    isSolution   :: Choice m s -> Bool

    solutions    :: s -> [Choice m s]
    solutions s  = filter isSolution $ spaceBy strategy s

    solution     :: m -> s -> Choice m s
    solution _ s = head . solutions $ s

depthFirst, breadthFirst :: Strategy m s
depthFirst               = (++)
breadthFirst             = flip (++)

-- | example of use
instance SearchProblem Int Int where
    translate a = [(b, b) | b <- [a-1, a-2..0]]
    isSolution  = (==0) . snd
    strategy    = breadthFirst

data Toy   = Buzz | Woody | Rex | Hamm deriving (Eq, Show, Enum, Ord)
type Group = [Toy]
type Place = Either Group Group

toys :: Group
toys = [Buzz, Woody, Rex, Hamm]

time     :: Toy -> Int
time toy = case toy of
                Buzz  -> 5
                Woody -> 10
                Rex   -> 20
                Hamm  -> 25

duration :: [Group] -> Int
duration = sum . map (maximum . map time)

instance SearchProblem Group Place where
        translate s     = case s of
                            Left xs  -> [(gone, Right $ xs \\ gone) |
                                           a <- xs,
                                           b <- xs,
                                           a /= b,
                                           let gone = [a, b] ]
                            Right ys -> [([re], Left $ re:ys) | re <- toys \\ ys]

        strategy        = depthFirst -- this strategy is better

        isSolution (ms, p) = case p of
                               Left _   -> False
                               Right [] -> duration ms <= 60
                               _        -> False

main :: IO ()
main = print . fst . solution toys $ (Left toys :: Place)
