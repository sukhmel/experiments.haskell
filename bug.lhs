1. When `(:~)` is used with `set ... [on command ...]`, program crashes, and prints `Segmentation fault/access violation in generated code` message;
2. Crashes occur frequently, but not always;
3. In case when tested with only one object, crash **never** happens immediately after start;
4. Upon use of `(:=)`, everything works fine;
5. Sample code is provided.

This issue was fully decribed in HeinrichApfelmus/reactive-banana#62.

* when line with `(:=)`, marked with a comment, is used, everything is perfectly fine
* when `(:~)` is used, random crashes occur frequently.

```haskell

> import Control.Monad (replicateM, mapM_)
> import Graphics.UI.WX

> main = start $ do
>        pad     <- frame [ ]                     
>        buttons <- replicateM 2
>                 $ button pad  []

>        mapM_ (flip set [on command := return ()]) buttons -- OK
>        mapM_ (flip set [on command :~ const (return ())]) buttons

```