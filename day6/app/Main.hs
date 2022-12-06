module Main where
import Data.List (nub)

prefix :: Eq a => [a] -> Int -> Int
prefix a n | nub (take n a) == take n a = n
           | otherwise = 1 + prefix (tail a) n

main :: IO ()
main = do 
    f <- readFile "./input.txt"
    print $ prefix f 4
    print $ prefix f 14

