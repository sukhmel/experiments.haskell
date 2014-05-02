module Main where

import Data.Char (toUpper)
import Control.Monad (forever)
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)
import Reactive.Banana
import Reactive.Banana.Frameworks

type Octave = Int

data Pitch = PA | PB | PC | PD | PE | PF | PG
             deriving (Eq, Enum, Bounded)

pitchChars :: [(Pitch, Char)]
pitchChars = [(p, toEnum $ fromEnum 'A' + fromEnum p) |
                           p <- [minBound .. maxBound]]

charPitches :: [(Char, Pitch)]
charPitches = map (\(a, b) -> (b, a)) pitchChars

data Note = Note Octave Pitch

instance Show Pitch where
    show p = case lookup p pitchChars of
                  Nothing -> error "impossible pitch"
                  Just c  -> [c]

instance Show Note where
    show (Note o p) = show p ++ show o

filterMapJust :: (a -> Maybe b) -> Event t a -> Event t b
filterMapJust f = filterJust . fmap f

changeOctave :: Int -> Octave -> Octave
changeOctave d = max 0 . min 10 . (d+)

getOctaveChange :: Char -> Maybe Int
getOctaveChange c = case c of
                         '+' -> Just 1
                         '-' -> Just (-1)
                         _   -> Nothing

makeNetworkDescription :: Frameworks t
                       => AddHandler Char
                       -> Moment t ()
makeNetworkDescription addKeyEvent = do
    eKey <- fromAddHandler addKeyEvent
    let eOctaveChange = filterMapJust getOctaveChange eKey
        bOctave       = accumB 3 (changeOctave <$> eOctaveChange)
        ePitch        = filterMapJust (`lookup` charPitches) eKey
        bPitch        = stepper PC ePitch
        bNote         = Note <$> bOctave <*> bPitch
    eNoteChanged <- changes bNote
    reactimate' $ fmap (putStrLn . ("Now playing " ++) . show)
               <$> eNoteChanged

main :: IO ()
main = do
    (addKeyEvent, fireKey) <- newAddHandler
    network <- compile (makeNetworkDescription addKeyEvent)
    actuate network
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    forever (fmap toUpper getChar >>= fireKey)



