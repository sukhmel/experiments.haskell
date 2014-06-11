
{-# LANGUAGE ScopedTypeVariables #-}

module Main (
    main
) where

import Control.Monad
import Data.List (transpose, subsequences)
import Data.Maybe
import Data.Traversable (sequenceA)
import Data.List.Grouping (splitEvery)

import Graphics.UI.WX hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

main :: IO ()
main = start mainFrame

mainFrame :: IO ()
mainFrame = do
       pad      <- frame [ text := "Mahjong calculator" ]
       noText <- staticText pad [text := ""]
       east   <- replicateM 4 $ button pad [text := ""]
       users  <- replicateM 4 $ textEntry pad [alignment := AlignCentre]
       score <- replicateM 4 $ textEntry pad [ text := ""
                                              , alignment := AlignRight
                                              -- , fontUnderline := True
                                              -- , fontWeight := WeightBold
                                              ]
       deltas <- mapM (\ a -> textEntry pad [ enabled := False
                                           , alignment := a ])
               $ concat [replicate 12 AlignRight, replicate 4 AlignCentre]
       winner <- replicateM 4 $ button pad [text := ""]
       sumAll <- replicateM 4 $ textEntry pad [text := "0"
                                              , enabled := False
                                              , alignment := AlignCentre]

       addNow <- button pad [text := "Sum current and overall"]
       undo   <- button pad [text := "Undo"]

       let box = splitInGroupsOf 4
               . concat
               . zipWith (:) score
               $ splitInGroupsOf 4 deltas
           inter = init box
           total = last box
       set pad [ layout := column 10
                         [ row 0
                         $ (\ e n c t w o
                           -> column 15 [e, n, column 0 c, t, w, o])
                         `map` map (centre . widget) east
                         `z` map widget users
                         `z` ( transpose
                             . map (map widget) ) inter
                         `z` map (centre . widget) winner
                         `z` map widget total
                         `z` map widget sumAll
                         , centre
                         . stretch
                         . row 15
                         . map widget
                         $ [addNow, undo]
                         , widget noText] ]


       let networkDescription :: forall t. Frameworks t => Moment t ()
           networkDescription = do
               -- convert WxHaskell events to FRP events
               let eventify            :: (Commanding w)
                                    => [w]
                                    -> Moment t [Event t ()]
                   eventify = mapM (\x -> event0 x command)

               eastEvents  <- eventify east
               winEvents   <- eventify winner
               undoEvent   <- event0 undo   command
               sumEvent    <- event0 addNow command

               userBehav   <- fmap ( ( ( proceed
                                       . Users
                                       . mapMaybe notEmpty
                                       . zip [0..]) <$>)
                                  . sequenceA)
                            . mapM (\ x -> behaviorText x "")
                            $ users

               scoreBehav  <- fmap ( ( ( proceed
                                       . Score
                                       . map ( maybe 0 id
                                             . readV) ) <$>)
                                   . sequenceA)
                            . mapM (\ x -> behaviorText x "")
                            $ score

               let behs = (\ a b c -> proceed c . a . b )
                       <$> userBehav
                       <*> scoreBehav
                   wind = foldl1 union
                        . zipWith (<$) (map East [0..])
                        $ eastEvents
                   lead = foldl1 union
                        . zipWith (<$) (map Wins [0..])
                        $ winEvents
                   undE = Undo  <$ undoEvent
                   sumE = SumUp <$ sumEvent

                   full = wind
                        `union` lead
                        `union` undE
                        `union` sumE

                   state = accumB initialState $ apply behs full

               stateChanges <- changes state
               reactimate' $ fmap print <$> stateChanges
               -- | Update visible state of buttons: enable possible
               -- variants, disable others, highlight current selection
               let updateGui   :: Behavior t State
                               -> Moment t ()
                   updateGui beh = do
                       let up t f = mapM_ (\ (b,i)
                                  -> sink b [text :== f i <$> beh])
                                  $ zip t [0..]
                           has s f i x = if i == f x then s else ""
                       up winner $ has "Won!" winPos
                       up east   $ has "East" eastPos
                       up total  $ (\ i s -> show $ totals s !! i)
                       up score  $ (\ i s -> show $ scores s !! i)
                       up sumAll $ (\ i s -> show $ overall s !! i)
                       up (take 12 deltas) $ (\ i s -> show
                                           . (!!i)
                                           . results
                                           $ s)

               updateGui state

       network <- compile networkDescription
       actuate network

mapIOtoEvent :: Frameworks t => (a -> IO b) -> Event t a -> Moment t (Event t b)
mapIOtoEvent f e1 = do
    (e2, fire2) <- liftIO newAddHandler
    reactimate $ (\x -> f x >>= fire2) <$> e1
    fromAddHandler e2

-- | Split a list into groups of N and take only those with exact size of N.
splitInGroupsOf :: Int -> [a] -> [[a]]
splitInGroupsOf n = takeWhile ((n ==) . length)
                  . splitEvery n

z ::  [b -> c] -> [b] -> [c]
z = zipWith ($)

type Player = Int
data State = State { eastPos :: Player
                   , scores  :: [Int]
                   , userSet :: [Player]
                   , winPos  :: Player
                   , totals  :: [Int]
                   , results :: [Int]
                   , overall :: [Int]
                   , history :: [State]
                   }
        deriving Show

data Setting   = East  Player
               | Score [Int]
               | Users [Player]
               | Wins  Player
               | SumUp
               | Undo
            deriving (Show, Eq)

initialState :: State
initialState = State 0 four [] 0 four (replicate 12 0) four []
    where four = replicate 4 0

proceed :: Setting -> State -> State
proceed got s = if got /= Undo
                    then updated -- { history = s : history s}
                    else s'
        where s'      = case got of
                             East  p -> s {eastPos = p}
                             Score v -> s {scores  = v}
                             Users p -> s {userSet = p}
                             Wins  p -> s {winPos  = if p == winPos s
                                                       then -1
                                                       else  p}
                             SumUp   -> initialState
                                          { eastPos = eastPos s
                                          , history = history s
                                          , overall = zipWith (+)
                                                       (totals s)
                                                       (overall s)}
                             Undo    -> case history s of
                                             (h:_) -> h
                                             _     -> s
              updated = s' { results = calc
                           , totals  = total }
              won     = scores s' !! pos
              pos     = winPos s'
              scr     = scores s'
              est     = eastPos s'
              use     = userSet s'
              calc    = catMaybes datum
              total   = foldl1 (zipWith (+))
                      . splitInGroupsOf 4
                      . map (fromMaybe 0)
                      $ datum
              datum   = map (\ (i, r) -> if fst i == snd i
                                           then Nothing
                                           else Just r)
                      . map (\ (i, r) -> if fst i `notElem` use ||
                                            snd i `notElem` use
                                            then (i, 0)
                                            else (i, r))
                      . map (\ (i, r) -> if est == fst i || est == snd i
                                            then (i, r*2)
                                            else (i, r ))
                      . map (\ (i, r) -> if pos == fst i
                                        then (i, -won)
                                        else if pos == snd i
                                                then (i, won)
                                                else (i, r))
                      . zip indices
                      . concat
                      . transpose
                      . splitInGroupsOf 4
                      $ liftM2 (-) scr scr
              indices = liftM2 (,) [0..3] [0..3]

readV s = listToMaybe [x | (x,"") <- reads s]

notEmpty :: (a, [b]) -> Maybe a
notEmpty (a, s) = a <$ listToMaybe s