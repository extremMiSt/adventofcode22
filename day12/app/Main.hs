{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Main where
import qualified Data.Set as S
import qualified Data.Sequence as Q

import Data.Char (ord)
import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromJust)
import Data.Sequence (Seq((:<|)), (><))

class Ord a => Graph b a where 
    neighbours :: b -> a -> [a]

instance Graph [String] (Int, Int) where
    neighbours :: [String] -> (Int, Int) -> [(Int, Int)]
    neighbours g (x,y) = l1 ++ l2 ++ l3 ++ l4
        where 
            l1 = [(x - 1, y) | ((x - 1) >= 0)               && ((height' g (x, y) + 1) >= height' g (x - 1, y))]
            l2 = [(x + 1, y) | ((x + 1) < length (g!!y))    && ((height' g (x, y) + 1) >= height' g (x + 1, y))]
            l3 = [(x, y - 1) | ((y - 1) >= 0)               && ((height' g (x, y) + 1) >= height' g (x, y - 1))]
            l4 = [(x, y + 1) | ((y + 1) < length g)         && ((height' g (x, y) + 1) >= height' g (x, y + 1))]

-- type where a is annotated with b
data An a b = An a b
    deriving (Show)

load :: An a b -> a
load (An a b) = a

annot :: An a b -> b
annot (An a b) = b

instance Eq a => Eq (An a b) where
    (==) :: Eq a => An a b -> An a b -> Bool
    An a b == An c d = a == c

instance Ord a => Ord (An a b) where
  compare :: Ord a => An a b -> An a b -> Ordering
  compare (An a b) (An c d) = compare a c

pathBFS :: Graph b a => b -> a -> S.Set (An a a) -> Q.Seq (An a a) -> S.Set (An a a)
pathBFS graph goal visited Q.Empty = S.empty
pathBFS graph goal visited (e:<|q) | e `S.member` visited = pathBFS graph goal visited q
                                   | e == An goal goal = S.insert e visited
                                   | otherwise = pathBFS graph goal (S.insert e visited) (q >< Q.fromList (map (\x -> An x (load e)) (neighbours graph (load e))))

getPath :: (Eq a, Ord a) => S.Set (An a a) -> a -> a -> [a]
getPath set element stop | An element element `S.notMember` set = []
                      | element == annot elem = []
                      | otherwise = element : getPath set (annot elem) stop
                        where 
                            elem = fromJust (S.lookupLE (An element element) set)

path :: Graph b a => b -> a -> a -> [a]
path graph start end = p
    where 
        p = getPath set end start
        set = pathBFS graph end S.empty (Q.singleton (An start start))

height' :: [String] -> (Int,Int) -> Int
height' map (x,y) | (x < 0) || (y < 0) || (y>=length map) || (x>=length (map!!y)) = 1000
                 | (map!!y)!!x == 'S' = ord 'a'
                 | (map!!y)!!x == 'E' = ord 'z'
                 | otherwise = ord ((map!!y)!!x)

findLetter :: [String] -> Char -> Int -> (Int, Int)
findLetter (s:map) c r = if c `elem` s
    then (fromJust (elemIndex c s),r)
    else findLetter map c (r+1)

allLetter :: [String] -> Char -> Int -> [(Int, Int)]
allLetter [] c r = []
allLetter (s:map) c r = zip (elemIndices c s) (repeat r) ++ allLetter map c (r+1) 

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let s = findLetter (lines f) 'S' 0
    let e = findLetter (lines f) 'E' 0

    print $ length (path (lines f) s e)

    let as = allLetter (lines f) 'a' 0
    print $ minimum (map length (filter (/= []) (map (\ss -> path (lines f) ss e) as)))

    putStrLn "done"
