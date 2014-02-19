import Control.Applicative
import Control.Monad
import Data.List

data KnightPos = KP Int Int KnightPos | Start deriving Show
instance Eq KnightPos where
    (KP lx ly lk) == (KP rx ry rk) = lx == rx 
                                  && ly == ry 
                               -- && lk == rk
 -- Start         == Start         = True
    _             == _             = False
    
moveKnight :: KnightPos -> [KnightPos]
moveKnight p@(KP c r _) = do
    (c', r', p') <- [(a, b, p) | (a, b) <- [ (c+2, r-1), (c+2, r+1)
                                           , (c-2, r-1), (c-2, r+1)
                                           , (c+1, r-2), (c+1, r+2)
                                           , (c-1, r-2), (c-1, r+2)
                                           ] ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return $ KP c' r' p'

getsIn3 :: KnightPos -> KnightPos -> [KnightPos]
getsIn3 from to = filter (== to) result 
                  where result = return from 
                                     >>= moveKnight 
                                     >>= moveKnight 
                                     >>= moveKnight

flatten   :: KnightPos -> [KnightPos]
flatten Start      = []
flatten (KP c r k) = (KP c r Start) : flatten k

moveSomewhereUnique      :: KnightPos -> [KnightPos]
moveSomewhereUnique from = filter once result
                           where result = return from >>= moveKnight
                                 once   = (==) <$> 
                                              length . flatten <*>
                                              length . nub . flatten
                                 
makeNUniqueSteps    :: Int -> KnightPos -> [KnightPos]
makeNUniqueSteps 0 a = return a
makeNUniqueSteps n k = result >>= makeNUniqueSteps (n-1)
                       where result     = nubBy unique $
                                          return k >>= moveSomewhereUnique 
                             unique l r = (0 ==) . length $
                                          intersect (flatten l) (flatten r)
