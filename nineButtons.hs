{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
-- import Data.Array
-- import Data.List hiding (union)
import Data.List.Grouping (splitEvery)

import Graphics.UI.WX hiding (Event)
-- import Graphics.UI.WXCore hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

splitInGroupsOf :: Int -> [a] -> [[a]]
splitInGroupsOf n = takeWhile ((n ==) . length)
                  . splitEvery n

main :: IO ()
main = start buttonPad

buttonPad ::  IO ()
buttonPad = do
       pad    <- frame [ text := "[1..9]" ]
       noText <- staticText pad [text := ""]
       btns   <- mapM (\n -> button pad
                             [ size := sz 40 40
                             , text := [n]])
                 ['1'..'9']
       set pad [ layout := column 5
                    [ grid 0 0
                    . map (map widget)
                    . splitInGroupsOf 3
                    $ btns
                    , widget noText ]]
       let networkDescription :: forall t. Frameworks t => Moment t ()
           networkDescription = do
               -- convert WxHaskell events to FRP events
               let eventify         :: (Commanding w) => [w] -> Moment t [Event t ()]
                   eventify widgets = forM widgets $ \x -> event0 x command

               events <- eventify btns

               let chosen :: Event t Int
                   chosen = foldl1 union
                          . zipWith (<$) [1..]
                          $ events

                   bSet   = stepper 0 chosen
               eSet   <- changes bSet
               reactimate' $ fmap print
                          <$> eSet

       network <- compile networkDescription
       actuate network
