{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import qualified Data.Set as S
import qualified Data.Set.Extra as SE
import qualified Data.Sequence as Q
import Data.Sequence (Seq((:<|)), (><))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Pos = (Integer,Integer)
data Dir = U|D|L|R
    deriving (Eq,Ord,Show)

parse :: [Char] -> Integer -> Integer -> [(Pos,Dir)]
parse [] _ _ = []
parse ('\n':xs) x y = parse xs 0 (y+1)
parse ('#':xs) x y = parse xs x y
parse ('>':xs) x y = ((x,y),R) : parse xs (x+1) y
parse ('<':xs) x y = ((x,y),L) : parse xs (x+1) y
parse ('^':xs) x y = ((x,y),U) : parse xs (x+1) y
parse ('v':xs) x y = ((x,y),D) : parse xs (x+1) y
parse (_:xs) x y = parse xs (x+1) y

blizzard :: Integer -> Integer -> (Pos,Dir) -> (Pos,Dir)
blizzard w h ((x,y),d) = case d of
    U -> ((x,if (y-1)<0 then h-1 else y-1),d)
    D -> ((x,if (y+1)>=h then 0 else y+1),d)
    L -> ((if (x-1)<0 then w-1 else x-1,y),d)
    R -> ((if (x+1)>=w then 0 else x+1,y),d)

blizzards :: Integer -> Integer -> [(Pos, Dir)] -> [(Pos, Dir)]
blizzards w h = map (blizzard w h)

draw :: S.Set Pos -> Integer -> Integer -> Integer -> Integer -> String
draw pos w h x y | x==w && h==y = ""
draw pos w h x y | x==w = '\n' : draw pos w h 0 (y+1)
draw pos w h x y = (if (x,y) `S.member` pos then '@' else ' ') : draw pos w h (x+1) y

class Ord a => Graph b a where 
    neighbours :: b -> a -> [a]

instance Graph (S.Set Pos, Integer, Integer) Pos where
  neighbours :: (S.Set Pos, Integer, Integer) -> Pos -> [Pos]
  neighbours (set,w,h) (x,y) = wait ++ up ++ down ++ left ++ right ++ exit ++ start
    where 
        wait = [(x,y) | S.notMember (x,y) set]
        up = [(x,y-1) | (y-1 >= 0) && S.notMember (x,y-1) set && nstart]
        down = [(x,y+1) | (y+1 < h) && S.notMember (x,y+1) set && ngoal]
        left = [(x-1,y) | (x-1 >= 0) && S.notMember (x-1,y) set && nstart && ngoal]
        right = [(x+1,y) | (x+1 < w) && S.notMember (x+1,y) set && nstart && ngoal]
        start = [(0, -1) | (x,y) == (0, 0)]
        exit = [(w-1, h) | (x,y+1) == (w-1, h)]
        nstart = (x,y) /= (0,-1)
        ngoal = (x,y) /= (w-1, h)

ns :: Graph b a => b -> a -> SE.Set a
ns g c = S.fromList (neighbours g c)

steps :: [(Pos,Dir)] -> Integer -> Integer -> S.Set (Integer,Integer) -> (Integer,Integer) -> Integer -> (Integer, [(Pos,Dir)])
steps b w h as g i = if done then (i+1, newB) else steps newB w h newAs g (i+1)
    where
        newB = blizzards w h b
        set = S.fromList (map fst newB)
        newAs = SE.concatMap (ns (set, w, h)) as
        done = g `elem` newAs

main :: IO ()
main = do  
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    --f <- readFile "./smallTest.txt"

    let blizz = parse (unlines.init.tail.lines $ f) 0 0
    let width = fromIntegral (length.head.lines $ f)-2
    let height = fromIntegral (length.lines $ f)-2

    let (toS, toB) = steps blizz width height (S.singleton (0,-1)) (width-1,height) 0
    print toS
    let (backS, backB) = steps toB width height (S.singleton (width-1,height)) (0,-1) toS
    print backS
    let (goalS, goalB) = steps backB width height (S.singleton (0,-1)) (width-1,height) backS
    print goalS

    putStrLn "done"