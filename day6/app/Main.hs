module Main where
import Data.List (nub)

prefix2 a n | nub (take n a) == take n a = n
          | otherwise = 1 + prefix2 (tail a) n

main :: IO ()
main = do 
    f <- readFile "./input.txt"
    print $ prefix2 f 14

