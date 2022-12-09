module Main where
import Data.List (elemIndex, intersect, nub)
import Data.Maybe (fromJust)

sameChar :: (String, String) -> Char
sameChar (x:xs, y) = if x `elem` y
    then x
    else sameChar (xs,y)

rank :: Char -> Integer
rank c = if c `elem` ['a'..'z']
    then toInteger (fromJust (elemIndex c ['a'..'z'])) + 1
    else toInteger (fromJust (elemIndex c ['A'..'Z'])) + 27

split :: String -> (String, String)
split s = splitAt (length s `div` 2) s

task1 :: String -> Integer
task1 s = sum $ map (rank . sameChar . split) (lines s)

same3 :: Eq a => ([a], [a], [a]) -> [a]
same3 (a,b,c) = a `intersect` b `intersect` c

tripple :: [c] -> [(c, c, c)]
tripple (x1:x2:x3:xs) = (x1,x2,x3) : tripple xs
tripple [] = []

task2 :: String -> Integer
task2 s = sum $ map (rank . head . same3 ) (tripple $ lines s)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    print $ task1 f
    print $ task2 f