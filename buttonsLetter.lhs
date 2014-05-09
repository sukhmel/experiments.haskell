Hello, everyone.

I'm trying to write a GUI for my program, using reactive-banana (specifically, reactive-banana-wx binding). In order to achieve my goal, I need to create main window with buttons, that trigger creation of a (modal) dialog window, where user should choose one of the given buttons again.

There are two problems: I can not get how to return collected value from a window, and I am not sure how to trigger that window.

Here's what I've got so far (modified TicTacToe.hs from reactive-banana-wx examples):

> {-# LANGUAGE ScopedTypeVariables #-}

> import Control.Monad
> import Data.List.Grouping (splitEvery)
> import Graphics.UI.WX hiding (Event)
> import Reactive.Banana
> import Reactive.Banana.WX

> splitInGroupsOf :: Int -> [a] -> [[a]]
> splitInGroupsOf n = takeWhile ((n ==) . length) . splitEvery n

> main :: IO ()
> main = start buttonPad

> buttonPad :: IO ()
> buttonPad = do
>        pad     <-frame [ text := "[1..9]" ]
>        noText  <- staticText pad [text := ""]
>        btns    <- mapM (\n -> button pad
>                              [ size := sz 40 40
>                              , text := [n]])
>                  ['1'..'9']
>        set pad [ layout := column 5
>                     [ grid 0 0
>                     . map (map widget)
>                     . splitInGroupsOf 3
>                     $ btns
>                     , widget noText ]]
>        let networkDescription :: forall t. Frameworks t => Moment t ()
>            networkDescription = do
>                -- convert WxHaskell events to FRP events
>                let eventify         :: (Commanding w) => [w] -> Moment t [Event t ()]
>                    eventify widgets = forM widgets $ \x -> event0 x command

>                events <- eventify btns

>                let chosen :: Event t Int
>                    chosen = foldl1 union
>                           . zipWith (<$) [1..]
>                           $ events

>                    bSet   = stepper 0 chosen
>                eSet   <- changes bSet
>                reactimate' $ fmap print
>                           <$> eSet

>        network <- compile networkDescription
>        actuate network

I've been struggling for quite a while with this, and would be very glad if someone points me to what I should learn to better cope with such problems.
It seems, that I should learn wxHaskell, and probably wxWidgets, but I'm not sure where to start.

Thanks in advance, Vlad.
