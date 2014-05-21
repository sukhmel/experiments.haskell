{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.List.Grouping (splitEvery)

import Graphics.UI.WX hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

splitInGroupsOf :: Int -> [a] -> [[a]]
splitInGroupsOf n = takeWhile ((n ==) . length)
                  . splitEvery n

main :: IO ()
main = start buttonPad

buttonPad ::  IO ()
buttonPad = do
       pad    <- frame [ text := "Sudoku frame" ]
       noText <- staticText pad [text := ""]
       btns   <- replicateM 9
               $ mapM (\n -> button pad
                             [ size := sz 40 40
                             , text := [n]])
                 ['1'..'9']
       set pad [ layout := column 5
                           [( grid 15 15
                            . map ( map ( grid 0 0
                                        . map ( map widget )
                                        . splitInGroupsOf 3))
                            . splitInGroupsOf 3) btns
                           , widget noText]
                    ]
       let networkDescription :: forall t. Frameworks t => Moment t ()
           networkDescription = do
               -- convert WxHaskell events to FRP events
               let eventify         :: (Commanding w)
                                    => [w]
                                    -> Moment t [Event t ()]
                   eventify widgets = forM widgets $ \x -> event0 x command

               events <- eventify . concat $ btns

               let chosen :: Event t (Int, Int)
                   chosen = foldl1 union
                          . zipWith (<$) [(x,y) | x <- [1..9]
                                                , y <- [1..9]]
                          $ events

                   bSet   = stepper (-1,-1) chosen
               eSet   <- changes bSet
               reactimate' $ fmap (print . positionToCoordinates)
                          <$> eSet

       network <- compile networkDescription
       actuate network

-- | Translate panelwise coordinates into cartesian (more or less)
-- e.g. what (1,1) (1,2) | (2,1) (2,2)    after   (0,0) (1,0) | (2,0) (3,0)
--      was: (1,3) (1,4) | (2,3) (2,4)    becomes (0,1) (1,1) | (2,1) (3,1)
--           ------------+------------            ------------+------------
--           (3,1) (3,2) | (4,1) (4,2)            (0,2) (1,2) | (2,2) (3,2)
--           (3,3) (3,4) | (4,3) (4,4)            (0,3) (1,3) | (2,3) (3,3)
positionToCoordinates :: (Int, Int) -> (Int, Int)
positionToCoordinates
          (glob, loc) = (x, y)
        where x       = 3*x1 + x0
              y       = 3*y1 + y0
              (y1,x1) = (glob - 1) `divMod` 3
              (y0,x0) = (loc  - 1) `divMod` 3
