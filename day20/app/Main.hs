module Main where
import Data.List (elemIndex, delete)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

mix :: (Eq b, Show b, Integral b) => [(Int, b)] -> [(Int, b)] -> [(Int, b)]
mix [] list = list
mix (a:as) list = mix as (trace (show complete) complete)
    where 
        complete = move list a (fst a)

moveLeft :: Eq a => [a] -> a -> [a] -> [a]
moveLeft l a [b] | a==b = a: init l
moveLeft l a (l1:l2:ls) | l1 /= a = l1: moveLeft l a (l2:ls)
moveLeft l a (l1:l2:ls) | l1 == a = l2:l1:ls

moveRight :: Eq a => [a] -> a -> [a] -> [a]
moveRight l a (l1:ls) | a==l1 = tail l ++ [a]
moveRight l a (l1:l2:ls) | l2 /= a = l1: moveRight l a (l2:ls)
moveRight l a (l1:l2:ls) | l2 == a = l2:l1:ls

move :: (Num a, Ord a, Eq t) => [t] -> t -> a -> [t]
move l a n | n == 0 = l
move l a n | n > 0 = move (moveLeft l a l) a (n-1)
move l a n | n < 0 = move (moveRight l a l) a (n+1)

main :: IO ()
main = do
    --f <- readFile "./input.txt"
    f <- readFile "./test.txt"
    let list = map read (lines f) :: [Int]
    let iList = zip list ([0..]::[Integer]) -- make them unique
    print iList
    print $ mix iList iList

    putStrLn "done"
