{- this is to become Brainfuck interpreter. As of now it can not get input in
   real-time and instead asks user to provide input beforehand                -}
module Main (
    main
) where

-- Next we are importing some things from other modules.
import Brainfuck
import System.IO (readFile)
import Control.Exception (catch)
import System.Environment (getArgs)


getInput script = do if ',' `elem` script
                        then do putStrLn "provide input:"
                                result <- getLine
                                return result
                        else return ""

{- exeMain : Executable Entry Point                                           -}
exeMain = do args <- getArgs
             case args of
                  [name] -> do
                      catch (do script <- readFile name
                                stdin <- getInput script
                                putStrLn $ getText $ runIOProgram script stdin)
                            ((\_ -> putStrLn "could not read file")
                                :: IOError -> IO ())
                  _      -> putStrLn "no script specified"

-------------------------------------------------------------------------------

main = exeMain
