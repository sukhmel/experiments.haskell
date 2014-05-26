{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Maybe
import Data.List.Grouping (splitEvery)

import Graphics.UI.WX hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

import Sudoku.Sudoku

main :: IO ()
main = start mainFrame

mainFrame ::  IO ()
mainFrame = do
       pad    <- frame [ text := "Sudoku frame" ]
       noText <- staticText pad [text := ""]
       btns   <- replicateM 9                     -- Selection buttons lets user
               $ mapM (\n -> button pad          -- navigate sudoku game.
                             [ size := sz 40 40
                             , text := [n]])
                 ['1'..'9']
       cbtns  <- mapM (\ n -> button pad          -- Choice buttons to let user
                            [ size := sz 40 40    -- choose  what  to  write in
                          , text := [n]])         -- currently selected cell.
                     ['1'..'9']
       set pad [ layout := column 15
                           [( grid 15 15
                            . map ( map ( grid 0 0
                                        . map ( map widget )
                                        . splitInGroupsOf 3))
                            . splitInGroupsOf 3) btns
                           , row 4
                           . map widget
                           $ cbtns
                           , widget noText]
                    ]

       let coordinates = [positionToCoordinates (x,y) | x <- [1..9]
                                                      , y <- [1..9]]
           buttons     = concat btns

           networkDescription :: forall t. Frameworks t => Moment t ()
           networkDescription = do
               -- convert WxHaskell events to FRP events
               let eventify         :: (Commanding w)
                                    => [w]
                                    -> Moment t [Event t ()]
                   eventify widgets = forM widgets $ \x -> event0 x command

               selectionEvents <- eventify buttons
               choiceEvents    <- eventify cbtns

               let selected :: Event t (Either (Int,Int) Int)
                   selected = foldl1 union
                          . zipWith (<$)
                          ( map Left coordinates)
                          $ selectionEvents

                   chosen :: Event t  (Either (Int, Int) Int)
                   chosen = foldl1 union
                          . zipWith (<$)
                          ( map Right [1..9])
                          $ choiceEvents

                   fullEvent = chosen `union` selected

                   moves :: Event t (Game -> Game)
                   moves = stepGame <$> fullEvent

                   eState :: Event t Game
                   eState = accumE (Nothing, Nothing, sudoku) moves

                   state :: Behavior t Game
                   state = stepper (Nothing, Nothing, sudoku) eState

                   -- | Update visible state of buttons: enable possible
                   -- variants, disable others, highlight current selection
                   update     :: Behavior t Game
                              -> Moment t ()
                   update beh = let sel (a,_,_) = fromMaybe (-1,-1) a
                                    cel (_,_,c) = c
                                in  do
                                    mapM_ (\ (w, i) ->
                                        let vals x | x'  ==  i =
                                                       (WeightBold  , red  )
                                                   | otherwise =
                                                       (WeightNormal, black)
                                                 where x' = sel x
                                            here = vals <$> beh
                                            textAt = valuesToLabel
                                                   . values
                                                   . flip getCell i
                                                   . cel
                                        in sink w [ text :== textAt <$> beh
                                                  , fontWeight :== fst <$> here
                                                  , textColor  :== snd <$> here
                                                  ])
                                     $ zip buttons coordinates
                                    mapM_ (\ (b,i) ->
                                           sink b [enabled :== elem i
                                                             . values
                                                             . liftM2 getCell
                                                               cel sel <$> beh])
                                          $ zip cbtns [1..]

               update state

       network <- compile networkDescription
       actuate network


-- | Split a list into groups of N and take only those with exact size of N.
splitInGroupsOf :: Int -> [a] -> [[a]]
splitInGroupsOf n = takeWhile ((n ==) . length)
                  . splitEvery n

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

valuesToLabel   :: [Int] -> String
valuesToLabel a = case a of
                       []  -> ""
                       [n] -> show n
                       _   -> "…"

type Game = (Maybe (Int,Int), Maybe Int, [Cell Int])

stepGame    :: Either (Int, Int) Int
            -> Game
            -> Game
stepGame a (c, v, task) = value'
         where value'   = if isJust c' && isJust v'
                             then ( Nothing
                                  , Nothing
                                  , step task (fromJust c')
                                              [fromJust v'])
                             else (c', v', task)
               (c', v') = case a of
                               Left  c'' -> (Just c'', v)
                               Right v'' -> (c, Just v'')
