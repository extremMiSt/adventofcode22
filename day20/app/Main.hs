module Main where
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.CircularList as C

mix ::[Integer] -> Integer -> Integer -> C.CList (Integer,Integer)
mix list factor = mixRounds (C.fromList tupAfac)
    where
        tupAfac = zip (map (*factor) list) [0..]
        mixRounds l 0 = l
        mixRounds l n = mixRounds (cMix tupAfac l) (n-1)

sumCoord :: (Eq a, Num a) => [a] -> a
sumCoord list = list!!((i+1000)`mod` length list) + list!!((i+2000)`mod` length list) + list!!((i+3000)`mod` length list)
    where 
        i = fromJust (elemIndex 0 list)

cMix :: (Eq b, Show b, Integral b) => [(Integer, b)] -> C.CList (Integer,b) -> C.CList (Integer,b)
cMix [] clist = clist
cMix (a:as) clist = cMix as complete
    where 
        complete = cMove clist a (fst a)

cMove :: (Eq t, Show t) => C.CList t -> t -> Integer -> C.CList t
cMove clist elem d = clist'
    where 
        focused = fromJust $ C.rotateTo elem clist
        removed = C.removeL focused
        rotated = C.rotN (fromInteger d `rem` length removed) removed
        clist' = C.insertL elem rotated

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let list = map read (lines f) :: [Integer]
 
    let task1 = C.toList (mix list 1 1)
    print $ sumCoord (map fst task1)

    let task2 = C.toList (mix list 811589153 10)
    print $ sumCoord (map fst task2)

    putStrLn "done"
