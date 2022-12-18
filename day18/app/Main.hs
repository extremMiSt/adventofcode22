{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence (Seq((:<|)), (|>), (><))
import Data.List (intersect)

type Cube = (Integer,Integer,Integer)
data Side = Side Cube Cube
    deriving (Show)

instance Eq Side where
  (==) :: Side -> Side -> Bool
  Side a b == Side c d = (a==c && b==d) || (a==d && b==c)

sides :: Cube -> [Side]
sides o@(a,b,c) = [Side o (a+1,b,c), Side o (a-1,b,c),
                              Side o (a,b+1,c), Side o (a,b-1,c),
                              Side o (a,b,c+1), Side o (a,b,c-1)]

parse :: [Char] -> Cube
parse s = triplet $ read ("["++s++"]")

triplet :: [Integer] -> Cube
triplet [a,b,c] = (a,b,c)

rdups :: Eq a => [a] -> [a]
rdups [] = []
rdups (x:xs) = if c then rdups (filter (/=x) xs) else x: rdups xs
    where
        c = x `elem` xs

boundsMax :: [Cube] -> (Integer,Integer,Integer)
boundsMax c = (x,y,z)
    where 
        x = maximum $ map (\(a,b,c) -> a) c
        y = maximum $ map (\(a,b,c) -> b) c
        z = maximum $ map (\(a,b,c) -> c) c

boundsMin :: [Cube] -> (Integer,Integer,Integer)
boundsMin c = (x,y,z)
    where 
        x = minimum $ map (\(a,b,c) -> a) c
        y = minimum $ map (\(a,b,c) -> b) c
        z = minimum $ map (\(a,b,c) -> c) c

class Ord a => Graph b a where 
    neighbours :: b -> a -> [a]

instance Graph (S.Set Cube) Cube where
  neighbours :: S.Set Cube -> Cube -> [Cube]
  neighbours set (a,b,c) = l1
    where 
        l1 = if ((a-1) >= -1) && S.notMember (a-1, b, c) set then (a-1, b, c) : l2 else l2
        l2 = if ((a+1) <= 20) && S.notMember (a+1, b, c) set then (a+1, b, c) : l3 else l3
        l3 = if ((b-1) >= -1) && S.notMember (a, b-1, c) set then (a, b-1, c) : l4 else l4
        l4 = if ((b+1) <= 20) && S.notMember (a, b+1, c) set then (a, b+1, c) : l5 else l5
        l5 = if ((c-1) >= -1) && S.notMember (a, b, c-1) set then (a, b, c-1) : l6 else l6
        l6 = [(a, b, c + 1) | ((c + 1) <= 20) && S.notMember (a, b, c + 1) set]

reacheableDFS :: (Graph b a) => b -> S.Set a -> Q.Seq a -> S.Set a
reacheableDFS graph visited Q.Empty = S.empty
reacheableDFS graph visited (e:<|q) | e `elem` visited = reacheableDFS graph visited q
                                    | otherwise = S.insert e (reacheableDFS graph (S.insert e visited) (q >< Q.fromList (neighbours graph e)))

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let faces = rdups $ concatMap (sides . parse) (lines f)
    print $ length faces

    let outside = reacheableDFS (S.fromList (map parse (lines f))) S.empty (Q.singleton ((-1,-1,-1)::Cube))
    let oFaces = rdups $ concatMap sides outside
    print $ length $ intersect oFaces faces

    putStrLn "done"
