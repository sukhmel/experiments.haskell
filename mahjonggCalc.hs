{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DefaultSignatures #-}

module Main (
    main
) where

import Control.Monad
import Data.List (transpose)
import Data.Maybe
import Data.Traversable (sequenceA)
import Data.List.Grouping (splitEvery)

import Graphics.UI.WX hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

import System.Environment (getArgs)
import Data.Serialize (Serialize, encode, decode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as B

getGame a = do game <- case a of
                            [file] -> B.readFile file
                                  >>= return
                                    . either (const def) id
                                    . decode
                            _      -> return def
               return game
    where def = (replicate 4 "", initialState)


main :: IO ()
main = getArgs >>= getGame >>= start . mainFrame

mainFrame :: ([String], State) -> IO ()
mainFrame (names, begin) = do
       pad      <- frame [ text := "Mahjong calculator" ]
       noText <- staticText pad [text := ""]
       east   <- replicateM 4 $ button pad [ text := ""
                                           , fontSize := 25
                                           , outerSize := sz 42 42 ]
       users  <- mapM (\ n -> textEntry pad [ text := n
                                            , alignment := AlignCentre
                                            , processEnter := True
                                            , processTab := True
                                            ]) names
       score  <- replicateM 4 $ textEntry pad [ text := ""
                                              , processEnter := True
                                              , processTab := True
                                              , alignment := AlignRight
                                              ]
       deltas <- mapM (\ a -> textEntry pad [ enabled := False
                                           , alignment := a ])
               $ concat [replicate 12 AlignRight, replicate 4 AlignCentre]
       winner <- replicateM 4 $ button pad [text := ""]
       sumAll <- replicateM 4 $ textEntry pad [ text := "0"
                                              , bgcolor := rgb 190 255 110
                                              , enabled := False
                                              , alignment := AlignCentre]

       addNow <- button pad [text := "Sum current and overall"]
       undo   <- button pad [text := "Undo"]
       save   <- button pad [text := "Save"]

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
                         . map (fill . stretch . widget)
                         $ [addNow, undo, save]
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
               textEvents  <- eventify $ concat [ users, score ]

               undoEvent   <- event0 undo   command
               sumEvent    <- event0 addNow command
               saveEvent   <- event0 save   command

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

               let wind = foldl1 union
                        . zipWith (<$) (map East [0..])
                        $ eastEvents
                   lead = foldl1 union
                        . zipWith (<$) (map Wins [0..])
                        $ winEvents
                   txtE = foldl1 union
                        $ textEvents
                   undE = Undo  <$ undoEvent
                   sumE = SumUp <$ sumEvent

                   allE = wind
                        `union` lead
                        `union` undE
                        `union` sumE
                   allB = (.) <$> userBehav <*> scoreBehav

                   setE = appl txtE `union` appl allE
                        where appl = apply ( const <$> allB )

                   state = accumB begin (union setE $ proceed <$> allE)

               -- | Save current game to user chosen file.
               reactimate $ fmap (\ s -> do n <- mapM (flip get text) users
                                            f <- fileSaveDialog pad True True
                                                "Choose a file to save game to:"
                                                [("Mahjong savegame",["*.mjg"])]
                                                "" "savegame.mjg"
                                            maybe (const $ return ())
                                                  (B.writeFile) f
                                             . encode . (,) n
                                             $ s { history = [] })
                          $ apply (const <$> state) saveEvent
               -- | Update visible state of buttons: enable possible
               -- variants, disable others, highlight current selection
               let updateGui   :: Behavior t State
                               -> Moment t ()
                   updateGui beh = do
                       let d w f p = mapM_ (\ (b,i)
                                   -> sink b [p :== f i <$> beh])
                                   $ zip w [0..]
                           up t f  = d t f text
                           sunrise = d east (\ i a -> if i == eastPos a
                                                        then rgb 230 90 80
                                                        else black) color
                           plays   = d score ((. userSet) . elem) enabled
                           has a f i x = if i == f x then a else ""
                       plays
                       sunrise
                       up east   $ (\ i s -> (!! ((i - eastPos s) `mod` 4))
                                           [ "東", "南", "西", "北" ])
                       up winner $ has "Won!" winPos
                       up total  $ (\ i s -> show $ totals s !! i)
                       up score  $ (\ i s -> showV $ scores s !! i)
                       up sumAll $ (\ i s -> show $ head (overall s) !! i)
                       up (take 12 deltas) $ (\ i s -> show
                                           . (!!i)
                                           . results
                                           $ s)

               updateGui state

       network <- compile networkDescription
       actuate network

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
                   , overall :: [[Int]]
                   , history :: [State]
                   }
        deriving (Show, Generic)

data Setting   = East  Player
               | Score [Int]
               | Users [Player]
               | Wins  Player
               | SumUp
               | Undo
            deriving (Show, Eq, Generic)

instance Serialize Setting
instance Serialize State

initialState :: State
initialState = State 0 four [] (-1) four (replicate 12 0) [four] []
    where four = replicate 4 0

proceed :: Setting -> State -> State
proceed got s = if got /= Undo
                    then updated { history = s : history s}
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
                                                       (head $ overall s)
                                                    : overall s  }
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
showV i = if i == 0 then "" else show i

notEmpty :: (a, [b]) -> Maybe a
notEmpty (a, s) = a <$ listToMaybe s
