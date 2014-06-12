There are three possible behaviours I observed:
  1) program crashes instantly, and prints error message
  2) program starts, but upon close prints same error
  3) program starts and closes fine
error message = Segmentation fault/access violation in generated code
when amount of buttons (see below) is one, (1) never occurs
upon increase of amount of buttons, (2) seems to occure less, than before
amount of [(1) or (2)] is estimated to be about 30-40 %

> {-# LANGUAGE ScopedTypeVariables #-}

> import Control.Monad (replicateM, mapM_, void)
> import Graphics.UI.WX hiding (Event)

> import Reactive.Banana
> import Reactive.Banana.WX

> main = start $ do
>        pad     <- frame [ ]                     
>        buttons <- replicateM 2
>                 $ button pad  []
>        let network :: forall t. Frameworks t => Moment t ()
>            network = void $ mapM_ (`event0` command) buttons
>                                 -- ^ crash inside ^
>        net <- compile network
>        actuate net