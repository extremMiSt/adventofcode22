{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Main where
import Data.List (sortOn)

lastN :: Int -> [a] -> [a]
lastN n xs = let m = length xs in drop (m-n) xs

doLine :: String -> [String] -> [([String], Integer, String)] -> ([([String], Integer, String)], [String])
doLine s curPath coll | head s == '$' && words s!!1 == "cd" && words s!!2 == ".." = (coll, tail curPath)
                    | head s == '$' && words s!!1 == "cd" = (coll, words s!!2:curPath)
                    | head s == '$' && words s!!1 == "ls" = (coll, curPath)
                    | words s !! 0 == "dir" = ((words s !! 1:curPath, 0, "dir"):coll,  curPath)
                    | otherwise = ((words s !! 1:curPath, read $ words s !! 0, "file"):coll, curPath)

doLines :: [String] -> [String] -> [([String], Integer, String)] -> [([String], Integer, String)]
doLines [] cur coll = coll
doLines (s:ss) cur coll = doLines ss next newColl
    where 
        (newColl, next) = doLine s cur coll

isInPath :: Eq a => [a] -> [a] -> Bool
isInPath path filePath = lastN (length path) filePath == path

sizeOf :: (Eq a2) => [a2] -> [([a2], Integer, String)] -> Integer
sizeOf path coll = sum (map (\(_,a,_)-> a) (filter (\(p, _, _) -> isInPath path p) coll))

task1 :: [String] -> Integer
task1 s = sum (map snd (filter ((<=100000).snd) sizes))
    where 
        sizes = map (\(p, _, _) -> (p, sizeOf p coll)) dirs
        dirs = filter (\(_,_,t)-> t=="dir") coll
        coll = doLines s ["/"] [(["/"],0,"dir")]

rootSize :: [([String], b, c)] -> b
rootSize ((p, l, t):ss) = if p == ["/"] then l else rootSize ss

task2 :: [String] -> Integer
task2 s = size
    where 
        sizes = map (\(p, _, t) -> (p, sizeOf p coll,t)) dirs
        dirs = filter (\(_,_,t)-> t=="dir") coll
        coll = doLines s ["/"] [(["/"],0,"dir")]
        free = 70000000 - rootSize sizes
        largeEnough = filter (\(_,s,t) -> ((free + s) >= 30000000) && t=="dir") sizes 
        (_,size,_) = head (sortOn (\(p,s,t)->s) largeEnough)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let f' = tail (lines f)
    print $ task1 f'
    print $ task2 f'
