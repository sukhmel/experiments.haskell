{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Maybe
import Data.List hiding (union)
import Data.List.Grouping (splitEvery)

import Graphics.UI.WX hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

import Sudoku.Sudoku

splitInGroupsOf :: Int -> [a] -> [[a]]
splitInGroupsOf n = takeWhile ((n ==) . length)
                  . splitEvery n

main :: IO ()
main = start mainFrame

mainFrame ::  IO ()
mainFrame = do
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

       cpad   <- dialog pad [text := "?"]
       cbtns  <- mapM (\ n -> button cpad
                            [ size := sz 40 40
                          , text := [n]])
                     ['1'..'9']
       set cpad [ layout := column 5
                          [ grid 0 0
                            . map ( map widget )
                            . splitInGroupsOf 3
                            $ cbtns] ]

       let networkDescription :: forall t. Frameworks t => Moment t ()
           networkDescription = do
               -- convert WxHaskell events to FRP events
               let eventify         :: (Commanding w)
                                    => [w]
                                    -> Moment t [Event t ()]
                   eventify widgets = forM widgets $ \x -> event0 x command

               localEvents <- eventify cbtns
               events      <- eventify . concat $ btns

               let chosen :: Event t (Either (Int,Int) Int)
                   chosen = foldl1 union
                          . zipWith (<$) [ Left
                                         . positionToCoordinates
                                         $ (x,y) | x <- [1..9]
                                                 , y <- [1..9]]
                          $ events

                   bSet   = stepper (Left (-1,-1)) chosen

               eSet   <- changes bSet

               sink cpad [visible :== (Left (-1,-1) /=) <$> bSet]

--               reactimate' $ fmap (const
--                           $ showModal cpad (\ s -> s $ Just 42) >> return ())
--                          <$> eSet
--
               let cEvent :: Event t  (Either (Int, Int) Int)
                   cEvent = foldl1 union
                          . zipWith (<$)
                          ( map Right [1..9])
                          $ localEvents

                   cBehav = stepper (-1)
                       . foldl1 union
                       . zipWith (<$) [1..9]
                       $ localEvents

                   fullEvent = union cEvent chosen

                   moves :: Event t (Game -> Game)
                   moves = (\ s -> stepGame s) <$> fullEvent

                   eState :: Event t Game
                   eState = accumE (Nothing, Nothing, sudoku) moves

                   state :: Behavior t Game
                   state = stepper (Nothing, Nothing, sudoku) eState


               changeState <- changes state

               let update = (\ beh ->
                             mapM_ (\ (w, i) ->
                                      sink w [ text :==
                                               valuesToLabel
                                             . values
                                             . flip getCell i
                                             . (\ (_,_,k) -> k)
                                            <$> beh])
                             $ zip (concat btns)
                                   [ positionToCoordinates
                                     (x,y) | x <- [1..9]
                                           , y <- [1..9]])
                             state
                   enable = mapM_ (\ (b,i) ->
                                   sink b [enabled :== (\ (_,_,lst) ->
                                                        either ( elem i
                                                               . values
                                                               . getCell lst)
                                                               ( const False))
                                                   <$> state <*> bSet])
                                  $ zip cbtns [1..]

               sink cpad [visible :== ((-1) /=) <$> cBehav]

               enable
               update

               reactimate' $ fmap ( printSudoku
                                  . \ (_,_,k) -> k) <$> changeState


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
