{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Main where

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
    spaceBy f b  = expand f $ step ([], b)
             where expand f []     = []
                   expand f (t:ts) = t : expand f (step t `f` ts)
                   step (ms, s)    = [(m:ms, s') | (m, s') <- translate s]
    
    strategy     :: Strategy m s
                 
    isSolution   :: Choice m s -> Bool
                 
    solutions    :: s -> [Choice m s]
    solutions s  = filter isSolution $ spaceBy strategy s

depthFirst, breadthFirst :: Strategy m s
depthFirst               = (++)
breadthFirst             = flip (++)

-- example of use
instance SearchProblem Int Int where
    translate a = [(b, b) | b <- [a-1, a-2..0]]
    isSolution  = (==0) . snd
    strategy    = breadthFirst
                  
main = print "none"
