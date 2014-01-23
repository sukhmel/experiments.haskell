{- this is code written while reading Gentle introduction to Haskell (russian
   translation) which happened to be my first book on Haskell.
   Text encoding is UTF-8, language is ru_RU                                  -}
                     -- see http://www.haskell.org/tutorial
                     -- ru: http://www.rsdn.ru/article/haskell/haskell_part1.xml‎

module Main (
    main
) where

import Data.Char (ord, chr)
import Control.Exception (catch)
import System.IO.Error (isEOFError)

{-============================================================================-}

{- рекурсивные определения списков                                            -}
{- бесконечная последовательность целых                                       -}
numsFrom   :: Int -> [Int]
numsFrom n = n:numsFrom(n+1)

{- последовательность чисел Фибоначчи                                         -}
fib = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib) ]

{- Последовательность чисел Фибоначчи получается, если приписать единицу к
результату почленного сложения самой этой последовательности и той же последова-
тельности, к которой приписан в начале нулевой элемент -}
                               -- http://akoub.narod.ru/funcbook/chapter2/c2.htm
fibn = 1 : zipWith (+) fibn (0 : fibn)

{- с использованием as-образца для последовательности. @ всегда ленивый: ~()  -}
fibt@(1:tfib) = 1 : 1 : [ a + b | (a, b) <- zip fibt tfib ]

{- бесконечный список простых чисел на основе _почти_ решета Эратосфена       -}
                           -- http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes = sieve [2..] where
            sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

{-============================================================================-}

{- вычисление длины списка, пример использования рекурсивного определения и : -}
len        ::  [a] -> Integer
len []     = 0
len (x:xs) = 1 + len xs

{-============================================================================-}

{- примеры задания типов                                                      -}

{- утянул откуда-то, интересно поразбираться с использованием не-ASCII        -}
data Tab a =      (:↺:)
           |     a :↓:   Tab a
           | Tab a :↙↘: (Tab a,Tab a)
           deriving (Eq, Show, Read)

{- пример пользовательского типа из Мягкого введения в Haskell                -}
                             -- http://rsdn.ru/article/haskell/haskell_part1.xml
data Tree a = Leaf a | Branch (Tree a) (Tree a)

{- конструкторы для этого типа, несколько кривые, всё же, но я не вижу смысла в
   их доработке -- лучше парсить строку для получения дерева (см ниже)        -}
makeBranch [x]       []  = Leaf x
makeBranch [x, y]    []  = makeBranch [x] [y]
makeBranch (x:y:xs)  []  = makeBranch [x,y] xs
makeBranch []       [x]  = Leaf x
makeBranch []    [x, y]  = makeBranch [x] [y]
makeBranch []  (x:y:xs)  = makeBranch [x,y] xs

makeBranch x          y  = Branch (makeBranch x []) (makeBranch y [])
{- например, makeBranch ['1', '2', '3', '4'] ['5', '6'] конструирует следующее:
  1  2  3  4             что, на мой взгляд, довольно странно, поскольку корнем
   \/    \/   5  6       является ветвь, а не узел. Хотя если бы дерево было бо-
    \____/     \/        лее обобщённым, то выглядело более похоже на ожидания
       \_______/                                                              -}

{-============================================================================-}

{- рекурсивное разворачивание дерева в массив -}
fringe                     :: Tree a -> [a]
fringe (Leaf x)            = [x]
fringe (Branch left right) = fringe left ++ fringe right

{-============================================================================-}

{- пример задания "воплощения"; если бы расширяли, то:(Eq a) => Tree a        -}
instance (Eq a) => Eq (Tree a) where
    Leaf a       == Leaf b         = a == b
    (Branch l r) == (Branch l' r') = (l==l') && (r==r')
    _            == _              = False
{- * здесь также будет реализован метод по-умолчанию:
    x            /= y              = not (x == y)                             -}
--------------------------------------------------------------------------------
{- функции вывода -}
{- функция, преобразующая дерево в строку                                     -}
showTree              :: (Show a) => Tree a -> String
showTree (Leaf x)     =  show x
showTree (Branch l r) =  "<" ++ showTree l ++ "|" ++ showTree r ++ ">"

{- функция, возвращающая "генератор" вывода, даёт линейную зависимость от длины
   дерева против квадратичной при выводе в строку с использованием showTree   -}
showsTree              :: Show a => Tree a -> String -> String
showsTree (Leaf x)     = shows x
showsTree (Branch l r) = ('<':) . showsTree l . ('|':) . showsTree r . ('>':)
--------------------------------------------------------------------------------
{- парсер строки в дерево. Можно использовать ReadS a = String -> [(a, String)-}
readsTree         :: (Read a) => String -> [(Tree a, String)]
readsTree ('<':s) =  [(Branch l r, u) | (l, '|':t) <- readsTree s,
                                        (r, '>':u) <- readsTree t ]
readsTree s       =  [(Leaf x, t)     | (x,t)      <- reads s]

{- свой lexer -}
lexAll :: ReadS String
lexAll s = case lex s of
            [("",_)] -> []
            [(c, r)] -> if length c == 1 then [(c, r)]
                           else [(c, r), ([head s], tail s)]
            any_else -> any_else

{- то же на основе лексического анализатора lex.                              -}
                                  -- http://stackoverflow.com/questions/21161364
readsTree'    :: (Read a) => String -> [(Tree a, String)]
readsTree' s  = [(Branch l r, x) | ("<", t) <- lexAll s,
                                   (l, u)   <- readsTree' t,
                                   ("|", v) <- lexAll u,
                                   (r, w)   <- readsTree' v,
                                   (">", x) <- lexAll w ]
                ++
                [(Leaf x, t)     | (x, t)   <- reads s ]
--------------------------------------------------------------------------------
{- "прикрепление" к видам классов                                             -}
{- придаём способность к выводу на экран                                      -}
instance Show a => Show (Tree a) where
    showsPrec _ x = showsTree x

{- способность к вводу из строки                                              -}
instance Read a => Read (Tree a) where
    readsPrec _ s = readsTree' s

{- даём возможность изменять нижележащий элементарный тип                     -}
instance Functor Tree where
    fmap f (Leaf x)       = Leaf (f x)
    fmap f (Branch t1 t2) = Branch (fmap f t1) (fmap f t2)

{-============================================================================-}

{- быстрая сортировка -}
qsort []     = []
qsort (x:xs) = qsort [y | y <- xs, y < x]
               ++ [x] ++
               qsort [y | y <- xs, y >= x]

{-============================================================================-}

{- инвертирование списка                                                      -}
rev []     = []
rev (x:xs) = rev(xs) ++ [x]

{-============================================================================-}

{- примеры каррированных функций, ничего особо интересного                    -}
add     :: Num a => a -> a -> a
add x y = x + y

inc :: Num a => a -> a -- без объявления типа будет ошибка, например в inc 1.0
inc = add 1

{-============================================================================-}

{- применение условий на наличие сравнения на равенство                       -}
mesh     :: Eq t => (a -> t) -> [a] -> [(t,t)]
mesh f a = [(x, y) | x <- map f a, y <- map f a, x /= y]

{-============================================================================-}

{- проверка на попадание переменной x в первые x элементов последовательности
   здесь ограничена длина проверяемой части последовательности для допущения
   использования бесконечных списков                                          -}
part x ys = x `elem` [y | y <- take x ys]

{- применение сечения инфиксного оператора, скобки обязательны                -}
plus_one = (+1)

{- удвоение первого элемента, применён as-образец, s - псевдоним для x:xs     -}
double_first s@(x:xs) = x:s

{- применение подстановочного символа, не связывается, значение игнорируется  -}
take_some ([x,y])     = (x, y)
take_some (x:_:y:_) = (x, y) --

{- пример использования case-нотации;
    if e1 then e2 else e3
        в действительности является сокращением для:
    case e1 of True  -> e2
               False -> e3                                                    -}

my_take m ys = case (m,ys) of
                    (0,_)    -> []
                    (_,[])   -> []
                    (n,x:xs) -> x : my_take (n-1) xs


{- рекурсивное задание последовательности с использованием ленивых шаблонов   -}
cli_init    = 0
next resp   = resp
process req = req + 1
client init ~(resp:resps) = init : client (next resp) resps -- ~(...) ленивый
{- вычисления будут отложены до необходимости, но к моменту, когда таковая необ-
ходимость наступит, server уже изменит значение resp:resps, добавив в его начало
process init, затем client будет проинициализирован этим значением. -}
server       (req:reqs)   = process req : server reqs
reqs  = client cli_init resps
resps = server reqs

{-============================================================================-}

summ          :: Integer -> [Integer] -> Integer
summ 0 _      = 0
summ n (x:xs) = x + summ (n-1) xs

{-============================================================================-}

{- примеры использования let, where и отбивки                                 -}
{- явное указание разделения на блоки                                         -}
let_demo a b c d =
 let {
     ; y = a*b
     ; f x = (x+y)/y
     }
  in f c + f d

{- указание разделения на блоки с использованием отбивки                      -}
let_demo_s a b c d =
    let y   = a * b
        f x = (x + y)/y
    in  f c + f d

{- использование ограничителей и where                                        -}
test_guards x y | y > z  = x + y
                | y == c = 0
                | y == z = x * y
                | y < z  = x - y
              where (z, c) = (x*x, x)

{-============================================================================-}
{- округление с заданной точностью                                            -}
approx n x = fromIntegral (round (10**n*x)) / 10**n

{- примеры деревьев для использования в экспериментах с синтаксисом и пр.     -}
test_tree = Branch (Branch (Leaf '1') (Branch (Leaf '2') (Leaf '3'))) (Leaf '4')
                --  2  3 <<'1'|<'2'|'3'>>|'4'>
                -- 1 \/
                --  \/ 4                    не может быть сконструировано с ис-
                --   \/                     пользованием makeBranch

someTree  = makeBranch ['1', '2', '3', '4'] ['5', '6']
                    -- 1  2  3  4 <<'1'|'2'>|<'3'|'4'>>|<'5'|'6'>>
                    --  \/    \/   5  6
                    --   \____/     \/      параметры makeBranch:
                    --      \_______/           ['1', '2', '3', '4'] ['5', '6']

otherTree = makeBranch (fringe (someTree)) []
                    --       3  4   5  6 <<'1'|'2'>|<<'3'|'4'>|<'5'|'6'>>>
                    --  1  2  \/     \/
                    --   \/    \_____/      параметры makeBranch:
                    --    \_______/             ['1', '2'] ['3', '4', '5', '6']

{-============================================================================-}

{- использование ввода-вывода и пример do-нотации                             -}
myGetLine :: IO String

myGetLine =  do c <- getChar
                if c == '\n'
                   then return ""
                   else do l <- myGetLine
                           if c /= ' '
                              then return (c:l)
                              else return l

{- обработка ошибок                                                           -}
getChar'  :: IO Char
getChar'  =  getChar `catch` eofHandler 
             where eofHandler e = if isEOFError e 
                                     then return '\n' 
                                     else ioError e
getLine'  :: IO String
getLine'  =  getLine'' `catch` handler
             where getLine'' :: IO String
                   getLine'' = do c <- getChar'
                                  if c == '\n' 
                                     then return ""
                                     else do l <- getLine'
                                             return (c:l)
                    handler :: IOError -> IO String
                    handler err = return ("Error: " ++ show err)

{-============================================================================-}

{- точка входа в программу.  При запуске изнутри  Leksah, нельзя использовать
   ввод из stdin                                                              -}
main = do putStrLn("wow, such code")
