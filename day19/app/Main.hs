{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence (Seq((:<|)), (><))

class Ord a => Graph b a where 
    neighbours :: b -> a -> [a]

{-instance Graph (S.Set Cube) Cube where
  neighbours :: S.Set Cube -> Cube -> [Cube]
  neighbours set (a,b,c) = l1
    where 
        l1 = if ((a-1) >= -1) && S.notMember (a-1, b, c) set then (a-1, b, c) : l2 else l2
        l2 = if ((a+1) <= 20) && S.notMember (a+1, b, c) set then (a+1, b, c) : l3 else l3
        l3 = if ((b-1) >= -1) && S.notMember (a, b-1, c) set then (a, b-1, c) : l4 else l4
        l4 = if ((b+1) <= 20) && S.notMember (a, b+1, c) set then (a, b+1, c) : l5 else l5
        l5 = if ((c-1) >= -1) && S.notMember (a, b, c-1) set then (a, b, c-1) : l6 else l6
        l6 = [(a, b, c + 1) | ((c + 1) <= 20) && S.notMember (a, b, c + 1) set]
-}

reacheableBFS :: (Graph b a) => b -> S.Set a -> Q.Seq a -> S.Set a
reacheableBFS graph visited Q.Empty = S.empty
reacheableBFS graph visited (e:<|q) | e `elem` visited = reacheableBFS graph visited q
                                    | otherwise = S.insert e (reacheableBFS graph (S.insert e visited) (q >< Q.fromList (neighbours graph e)))

type Id = Integer
type Ore = Integer
type Clay = Integer
type Obsidian = Integer
type Blueprint = (Id, Ore, Ore, Ore, Clay, Ore, Obsidian)

parse :: String -> (Id, Ore, Ore, Ore, Clay, Ore, Obsidian)
parse s = (read $ init (token!!1), read (token!!6), read (token!!12), 
    read (token!!18), read (token!!21), read (token!!27), read (token!!30))
    where
        token = words s 

type Time = Integer
type Geode = Integer
type OreRobot = Integer
type ClayRobot = Integer
type ObsidianRobot = Integer
type GeodeRobot = Integer
type State = (Time, Ore, OreRobot, Clay, ClayRobot, Obsidian, ObsidianRobot, Geode, GeodeRobot)

instance Graph Blueprint State where
  neighbours :: Blueprint -> State -> [State]
  neighbours blue (t,o,ob,c,cb,b,bb,g,gb) = undefined
    where 
        t' = t+1
        o' = o + ob
        c' = c + cb
        b' = b + bb
        g' = g + gb


main :: IO ()
main = do
    f <- readFile "./input.txt"
    f <- readFile "./test.txt"

    print $ map parse (lines f)

    putStrLn "done"

