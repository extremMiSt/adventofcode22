{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence (Seq((:<|)), (><))
import Debug.Trace (trace)

class Ord a => Graph b a where 
    neighbours :: b -> a -> [a]

reacheableBFS :: (Graph b a) => b -> S.Set a -> Q.Seq a -> S.Set a
reacheableBFS graph visited Q.Empty = visited
reacheableBFS graph visited (e:<|q) | e `elem` visited = reacheableBFS graph visited q
                                    | otherwise = reacheableBFS graph (S.insert e visited) (q >< Q.fromList (neighbours graph e))

type Id = Integer
type Ore = Integer
type Clay = Integer
type Obsidian = Integer
type Blueprint = (Id, Ore, Ore, Ore, Clay, Ore, Obsidian)

getId :: Blueprint -> Id
getId (id, _,_,_,_,_,_) = id

getObOre :: Blueprint -> Id
getObOre (_,obOre,_,_,_,_,_) = obOre

getCbOre :: Blueprint -> Ore
getCbOre (_,_,cbOre,_,_,_,_) = cbOre

getBbOre :: Blueprint -> Ore
getBbOre (_,_,_,bbOre,_,_,_) = bbOre

getBbClay :: Blueprint -> Clay
getBbClay (_,_,_,_,bbClay,_,_) = bbClay

getGbOre :: Blueprint -> Ore
getGbOre (_,_,_,_,_,gbOre,_) = gbOre

getGbObs :: Blueprint -> Obsidian
getGbObs (_,_,_,_,_,_,gbObs) = gbObs

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

getGeodes :: State -> Geode
getGeodes (_,_,_,_,_,_,_,g,_) = g

instance Graph Blueprint State where
    neighbours :: Blueprint -> State -> [State]
    neighbours blue s@(t,o,ob,c,cb,b,bb,g,gb) = dbg2 s (keep ++ nob ++ ncb ++ nbb ++ ngb)
        where 
            t' = t + 1
            o' = o + ob
            c' = c + cb
            b' = b + bb
            g' = g + gb
            time = t<24
            obPoss = o == getObOre blue
            cbPoss = o >= getCbOre blue
            --bbPoss = ((o == getBbOre blue) && (c >= getBbClay blue)) || ((o >= getBbOre blue) && (c == getBbClay blue))
            bbPoss = (o >= getBbOre blue) && (c >= getBbClay blue)
            --gbPoss = ((o == getGbOre blue) && (b >= getGbObs blue)) || ((o >= getGbOre blue) && (b == getGbObs blue))
            gbPoss = (o >= getGbOre blue) && (b >= getGbObs blue)
            keep = [(t',o',ob,c',cb,b',bb,g',gb) | time]
            nob = [(t',o'-getObOre blue,ob+1,c',cb,b',bb,g',gb) | time && obPoss]
            ncb = [(t',o'-getCbOre blue,ob,c',cb+1,b',bb,g',gb) | time && cbPoss]
            nbb = [(t',o'-getBbOre blue,ob,c'-getBbClay blue,cb,b',bb+1,g',gb) | time && bbPoss]
            ngb = [(t',o'-getGbOre blue,ob,c',cb,b'-getGbObs blue,bb,g',gb+1) | time && gbPoss]

main :: IO ()
main = do
    --f <- readFile "./input.txt"
    f <- readFile "./test.txt"

    let blueprints = map parse (lines f)
    let possible = reacheableBFS (blueprints!!1) S.empty (Q.singleton (0,0,1,0,0,0,0,0,0))
    print $ head blueprints
    print $ maximum $ S.map getGeodes possible

    putStrLn "done"

dbg :: Show a => a -> a
dbg a = trace (show a ) a

dbg2 :: (Show a, Show m) => m -> a -> a
dbg2 m a = trace (show m ++ ": " ++ show a ) a