module Main where
import Data.Char (ord)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

lookNeighbor :: [String] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
lookNeighbor map ((x,y,l):q) = q ++ up ++ down ++ left ++ right
    where 
        height = height' map (x,y)
        up = [(x,y-1,l+1)   | (height + 1) <= height' map (x,y-1)]
        down = [(x,y+1,l+1) | (height + 1) <= height' map (x,y+1)]
        left = [(x-1,y,l+1) | (height + 1) <= height' map (x-1,y)]
        right = [(x+1,y,l+1)| (height + 1) <= height' map (x+1,y)]

height' :: [String] -> (Int,Int) -> Int
height' map (x,y) | (x < 0) || (y < 0) || (y>=length map) || (x>=length (map!!y)) = 1000
                 | (map!!y)!!x == 'S' = ord 'a'
                 | (map!!y)!!x == 'E' = ord 'z'
                 | otherwise = ord ((map!!y)!!x)

findS :: [String] -> Int -> (Int, Int)
findS (s:map) r = if 'S' `elem` s
    then (fromJust (elemIndex 'S' s),r)
    else findS map (r+1)

findR :: [String] -> [(Int,Int,Int)] -> Int
findR map q = if done 
    then l 
    else findR map ((y,x,l):ls)
        where
            ((y,x,l):ls) = lookNeighbor map q
            done = ((map!!y)!!x) == 'E'

main :: IO ()
main = do
    --f <- readFile "./input.txt"
    f <- readFile "./test.txt"
    let s@(x,y) = findS (lines f) 0
    print s
    print $ findR (lines f) [(x,y,0)]


