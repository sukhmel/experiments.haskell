{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Maybe
import Data.List.Grouping (splitEvery)

import Graphics.UI.WX hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

main  = start first

first = do
           parentPad   <- frame [text := "First window"]
           firstButton <- button parentPad [ text := "First" ]

           childPad  <- dialog parentPad [text := "Second window"]
           okButton  <- button childPad [text := "Allright"]
           noButton  <- button childPad [text := "No way"]
           set childPad [layout := row 20 $ map widget [okButton, noButton]]

           let netDescription :: forall t. Frameworks t => Moment t ()
               netDescription = do
                             event <- event0 firstButton command
                             let behav :: Behavior t Int
                                 behav = stepper (-1) $ 1 <$ event
                             sink childPad [visible :== (>0) <$> behav]
                             happenings <- mapM (flip event0 command) [okButton
                                                                      ,noButton]
                             let userChoice :: Behavior t Bool
                                 userChoice = stepper False
                                            . foldl1 union
                                            . zipWith (<$) [True, False]
                                            $ happenings

                             test <- changes ((\ x y ->
                                                       show x ++ show y)
                                                  <$> behav
                                                  <*> userChoice)
                             reactimate' $ fmap (print) <$> test

           network <- compile netDescription
           actuate network




