--------------------------------------------------------------------------------
--
-- Module      :  Brainfuck
-- Copyright   :  MIT
-- License     :  AllRightsReserved
--
-- Maintainer  :  sukhmel @ https://github.com/sukhmel
-- Stability   :  
-- Portability :  
-- 
-- Description : Brainfuck interpreter, emulating brainfuck-machine, that 
--               contains information about: 
--                  * loaded program
--                  * used amount (ever accessed) of memory, 
--                  * memory itself, represented by infinite list of Int
--                  * current position in memory
--                  * current position of interpretator carriage
--                  * execution flag -- True, when execution is completed
--                  * stdin, stdout strings. I still have not managed to make 
--                    this an IO Monad
--
-- Usage       :    import Brainfuck (brainfuck, getText, runEmbedded)
--                  hello = brainfuck "++++++++++[>+++++++>\
--                                    \++++++++++>+++>+<<<<\
--                                    \-]>++.>+.+++++++..++\
--                                    \+.>++.<<++++++++++++\
--                                    \+++.>.+++.------.---\
--                                    \-----.>+."  
--                  main = do putStrLn (getText $ runEmbedded hello)                          
-- | 
--
--------------------------------------------------------------------------------

module Brainfuck (
    getText, getProgram             -- get stdout / program contents
    , state, brainfuck, brainfuckIn -- default constructors
    , runIOBrainfuck,   runBrainfuck -- \
    , runIOEmbedded,    runEmbedded  -- | execution wrappers for convenience
    , runIOProgram,     runProgram   -- |
    , run                            -- /
) where

import Data.Char (ord, chr)

{- default constructor -- everything is empty                                 -}
state = State [0,0..] 0 0 0 False "" "" ""

{- run program with predefined input inside of a present instance of State    -}
runBrainfuck p io s = runEmbedded $ s {program = p, stdin = io}

{- run program inside of a present instance of State, interactively           -}
runIOBrainfuck p s      = runIOEmbedded $ s {program = p} 

{- run program that is embedded into State with predefined stdin              -}
runEmbedded s = let (s', i) = stepProg s 
                    s''     = case i of 
                                   Read -> continue (s' {stdin = cs}) (Just c)
                                   _    -> continue s' Nothing
                    (c:cs)  = stdin s'
                in if done s'' then s''
                               else runEmbedded s''

{- run program that is embedded into present instance of State, interactively -}
runIOEmbedded s = let (s', i) = stepProg s in
                  case i of
                       Write c -> do putChar c
                                     runIOEmbedded s'
                       Read    -> do c <- getChar
                                     runIOEmbedded $ continue s' (Just c)
                       None    -> if done s' then return s'
                                             else do runIOEmbedded s'

{- run program with predefined input inside of a default State                -}
runProgram p input = runBrainfuck p input state

{- run program inside of a default State, interactively                       -}
runIOProgram p = runIOBrainfuck p state
run            = runIOProgram       -- alias

{- Emulator state                                                             -}
data State =
    State {memory   :: [Int],
           position, 
           carriage, 
           max_pos  :: Int,
           done     :: Bool,
           program,
           stdin,
           stdout   :: [Char]}

{- interrupts as tools of interacting with user                               -}
data Interrupt = None | Read | Write Char

{- function to continue execution after interrupt (including None)            -}
continue            :: State -> Maybe Char -> State
continue s (Just i) = s' where
                           p  = program  s 
                           c  = carriage s
                           s' = case p !! c of
                                     ',' -> incCar $ setCMem i s
                                     _   -> s
continue s Nothing  = s 

{- getter for stdout of Brainfuck-machine                                     -}
getText   :: State -> [Char]
getText s = stdout s

{- getter for program in Brainfuck-machine                                    -}
getProgram   :: State -> [Char]
getProgram s =  program s

showsState (State mem pos car max done prog sin sout) dest =
    memory ++ delimiter
    ++ condition 
    ++ debug ++ delimiter
    ++ "IN: \""  ++ sin  ++ "\"" ++ delimiter
    ++ "OUT: \"" ++ sout ++ "\"" ++ delimiter
    ++ dest
    where
        debug     | done      = prog ++ "_"
                  | otherwise = x ++ '_':y:'_':ys
                where (x, y:ys) = splitAt car prog
                 
        condition | done      = " completed." ++ delimiter
                  | otherwise = " running..." ++ delimiter
          
        memory = init (show l) ++ left ++ show r ++ right ++ tail (show rs) 
                where (l, r:rs) = splitAt pos (take (max+1) mem)
                      left  = case length l of
                                   0 -> "_"
                                   _ -> ", _"
                      right = case length rs of
                                   0 -> "_"
                                   _ -> "_, "
        delimiter        = " "

instance Show State where
    showsPrec _ a = showsState a



{- constructor that takes program text and stdin text                         -}
brainfuckIn prog input = state {program = prog, stdin = input}

{- constructor that takes only program text                                   -}
brainfuck prog         = brainfuckIn prog "" 

{- upper level functions to treat changes in memory, and execution            -}
changeMemory, changePosition, changeCarriage :: (Int -> Int) -> State -> State

changeMemory f s   = let (x,y:ys) = splitAt (position s) (memory s)
                     in s {memory = x ++ (mod (f y) 256) : ys}
                     
changePosition f s = let p' = f p            -- new position
                         p  = position s     -- made of current one
                         m' | p' > m = p'    -- maximal memory position ever ac-
                            | True   = m        -- cessed,  this is used to dis-
                          where m = max_pos s   -- play only used memory
                     in s {position = p', max_pos = m'}

changeCarriage f s = let c' = f $ carriage s
                         d  = length p <= c' -- when  a carriage  is  past  last
                         p  = program  s     -- symbol of program -- it finishes
                     in s {carriage = c', done = d }    

{- convenient functions to change state condition                             -}
seekPrev, incMem, incPos, incCar, seekNext, 
          decMem, decPos, decCar        :: State -> State
incMem = changeMemory   ( 1+)
decMem = changeMemory   (-1+)
incPos = changePosition ( 1+)
decPos = changePosition (-1+)
incCar = changeCarriage ( 1+)
decCar = changeCarriage (-1+)

{- seek corresponding square bracket with respect to nesting                  -}
seekBrace     :: Int -> State -> State
seekBrace n s | n == 0 = s
              | n /= 0 = case p !! c of
                             '[' -> seekBrace (n+1) s'   -- will satisfy  condi-
                             ']' -> seekBrace (n-1) s'   -- tion or add nesting
                             _   -> seekBrace  n    s'   -- level
            where step | n <  0 = decCar            -- positive n looks forwards
                       | n >  0 = incCar            -- negative n  --  backwards
                  s' = step s
                  p  = program s
                  c  = carriage s'

{- old function to convert memory into string, left for educational puproses  -}
showMem   :: State -> String
showMem s | max >= p = (chr $ m !! p) : showMem s' 
          | True     = []  
        where m   = memory s
              p   = position s
              s'  = s {position = p+1}
              max = max_pos s 

{- convert ever accessed memory inside of state into string representation.   -}
showMem'   :: State -> String
showMem' s = map chr $ take len mem 
             where len = 1 + max_pos s
                   mem = memory  s

{- convenience functions used to match braces                                 -}
seekPrev s = seekBrace (-1) s
seekNext s = seekBrace   1  s

{- get/set memory at current position. _CMem functions convert to/from Char, be-
     cause memory itself should be represented as integers array, but simulation
     conditions reqire that all input/output is done as they were chars       -}
getMem   :: State -> Int
getMem s = (memory s) !! (position s)

setMem   :: Int -> State -> State
setMem i = changeMemory (\_ -> i)  

getCMem  :: State -> Char
getCMem  = chr . getMem

setCMem  :: Char -> State -> State
setCMem  = setMem . ord  

{- make one step of interpreter                                               -}
stepProg s = (s', i) where
                 p  = program  s 
                 c  = carriage s
                 s' = if done s then s
                           else 
                              case p !! c of
                                  '<' -> incCar $ decPos s
                                  '>' -> incCar $ incPos s
                                  '[' -> case getMem s of
                                              0 -> incCar $ seekNext s
                                              _ -> incCar s
                                  ']' -> case getMem s of
                                              0 -> incCar s
                                              _ -> seekPrev s
                                  '-' -> incCar $ decMem s
                                  '+' -> incCar $ incMem s
                                  '.' -> incCar $ s {stdout = o ++ [getCMem s]}
                                  ',' -> s
                                  _   -> incCar s
                                where (x:xs) = stdin s
                                      o      = stdout s
                 i  = case p !! c of -- interrupt
                           ',' -> Read
                           '.' -> Write $ getCMem s
                           _   -> None
