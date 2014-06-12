{-# LANGUAGE ScopedTypeVariables #-}

module Main (
    main
) where

import Control.Monad
import Data.Maybe
import Data.List.Grouping (splitEvery)

import Graphics.UI.WX hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

import Sudoku.Sudoku
import Sudoku.TaskReader

import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          task <- case args of
                      [file] -> readTask file
                      _      -> return sudoku
          start . mainFrame $ task

mainFrame :: [Cell Int] -> IO ()
mainFrame task = do
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
       auto   <- checkBox pad [text := "Auto update possibilities"]
       upd    <- button   pad [text := "Update now"]
       undo   <- button   pad [text := "Undo"]
       save   <- button   pad [text := "Save to file"]
       solve  <- button   pad [text := "Solve further"]
       set pad [ layout := column 15
                           [( grid 15 15
                            . map ( map ( grid 0 0
                                        . map ( map widget )
                                        . splitInGroupsOf 3))
                            . splitInGroupsOf 3) btns
                           , row 4
                           . map widget $ cbtns
                           , expand $ widget auto
                           , hstretch
                           $ row 4
                           . map ( expand
                                 . stretch )
                           $ [ widget upd
                             , widget undo
                             , widget save
                             ]
                           , expand $ widget solve
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
               saveEvent       <- event0 save command
               controlEvents   <- do a <- event0 auto command
                                     b <- eventify [ solve
                                                   , undo
                                                   , upd ]
                                     return (a:b)

               let selected :: Event t Move
                   selected = foldl1 union
                          . zipWith (<$)
                          ( map Select coordinates)
                          $ selectionEvents

                   chosen   :: Event t Move
                   chosen   = foldl1 union
                            . zipWith (<$)
                            ( map Choose [1..9])
                            $ choiceEvents

                   control :: Event t Move
                   control = foldl1 union
                            . zipWith (<$)
                            [ AutoUp, Solve, Undo, Update ]
                            $ controlEvents

                   fullEvent = chosen
                             `union` control
                             `union` selected

                   moves :: Event t (Game -> Game)
                   moves = stepGame <$> fullEvent

                   eState :: Event t Game
                   eState = accumE (begin task) moves

                   state :: Behavior t Game
                   state = stepper (begin task) eState

                   -- | Update visible state of buttons: enable possible
                   -- variants, disable others, highlight current selection
                   updateGui     :: Behavior t Game
                                 -> Moment t ()
                   updateGui beh =
                       let s = fromMaybe (-1,-1) . sel
                       in  do
                           mapM_ (\ (w, i) ->
                               let vals x = if s x == i
                                            then (WeightBold  , red  )
                                            else (WeightNormal, black)
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
                                                      cel s <$> beh])
                                 $ zip cbtns [1..]

               updateGui state
               
               reactimate $ fmap (\ s -> do name <- fileSaveDialog
                                                    pad True True
                                                    "Choose a file:"
                                                    [("Sudoku", ["*.sudoku"])]
                                                    "" "save.sudoku"
                                            maybe (const $ return ())
                                                  (writeFile) name
                                             . unlines
                                             . map ( unwords 
                                                   . map show)
                                             $ getValuesArray s)
                          $ apply (const <$> state) saveEvent

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

data Game = State { sel :: Maybe (Int,Int)
                  , val :: Maybe Int
                  , cel :: [Cell Int]
                  , hst :: [[Cell Int]]
                  , aut :: Bool}
            deriving Show

data Move = Select (Int, Int)
          | Choose Int
          | AutoUp
          | Update
          | Solve
          | Undo
            deriving Show

getValuesArray :: Game -> [[Int]]
getValuesArray g = map (map (cut . values . getCell cells)) coord
    where coord  = [[(x, y) | x <- [0..8]]
                            | y <- [0..8]]
          cells  = cel g
          cut cs = case cs of
                        [v] -> v
                        _   -> 0
            
begin ::  [Cell Int] -> Game
begin task = State Nothing Nothing task [] False

stepGame    :: Move
            -> Game
            -> Game
stepGame m s = r'
    where r' = if isJust c' && isJust v'
                  then s' { sel = Nothing
                          , val = Nothing
                          , cel = f t (fromJust c')
                                      [fromJust v']
                          , hst = t:h}
                  else s'
          f  = if a
                  then update
                  else step
          c' = sel s'
          v' = val s'
          t  = cel s
          h  = hst s
          a  = aut s
          (u,h') = case h of
                        (b:c) -> (b, c)
                        _     -> (t, h)
          s' = case m of
                  Select i -> s {sel = Just i}
                  Choose n -> s {val = Just n}
                  AutoUp   -> s {aut = not a}
                  Update   -> s {cel = update t (-1,-1) [], hst = t:h}
                  Solve    -> s {cel = getSolution t,       hst = t:h}
                  Undo     -> s {cel = u,                   hst =  h'}