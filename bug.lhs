> {-# LANGUAGE ScopedTypeVariables #-}

> import Control.Monad (replicateM, mapM_, void)
> import Graphics.UI.WX

> main = start $ do
>        pad     <- frame [ ]                     
>        buttons <- replicateM 2
>                 $ button pad  []
>        mapM_ (flip set [on command :=  putStrLn "test"]) buttons
>        net <- compile network
>        actuate net