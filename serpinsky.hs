module Main where

main :: IO ()
main = getContents
   >>= mapM_ ( putStrLn
             . concatMap showS)
     . (\ [w, n] -> triangle w n)
     . map read
     . words

showS :: Int -> String
showS 1 = "1"
showS _ = " "

triangle :: Int -> Int -> [[Int]]
triangle w n = map mirror
             . serpIter n
             . reverse
             . take w
             . map (take w)
             . iterate (0:)
             $ ones
    where ones = 1 : ones
          mirror x = x ++ (drop 1 . reverse) x

serpIter :: Int -> [[Int]] -> [[Int]]
serpIter 0 x = x
serpIter n x = serpIter (n - 1) a
            ++ serpIter (n - 1) (zipWith (zipWith (-)) b $ reverse a)
    where (a, b) = splitAt (length x `div` 2) x
