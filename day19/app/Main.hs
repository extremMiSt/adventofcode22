{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

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

reacheableBFS2 :: (Graph (b, S.Set a) a) => (b, S.Set a) -> S.Set a -> Q.Seq a -> S.Set a
reacheableBFS2 graph visited Q.Empty = visited
reacheableBFS2 graph visited (e:<|q) | e `elem` visited = reacheableBFS2 (fst graph, visited) visited q
                                    | otherwise = reacheableBFS2 (fst graph, visited) (S.insert e visited) (q >< Q.fromList (neighbours graph e))


reacheableDFS :: (Graph b a) => b -> a -> S.Set a
reacheableDFS graph current | null (neighbours graph current) = S.singleton current
                            | otherwise = S.unions (S.map (reacheableDFS graph) (S.fromList (neighbours graph current)))

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

instance Graph  (Blueprint, S.Set State) State where
    neighbours :: (Blueprint, S.Set State) -> State -> [State]
    neighbours blue s@(t,o,ob,c,cb,b,bb,g,gb) = dbg2 s (keep ++ nob ++ ncb ++ nbb ++ ngb)
        where 
            t' = t + 1
            o' = o + ob
            c' = c + cb
            b' = b + bb
            g' = g + gb
            time = t<24
            obPoss = o >= getObOre (fst blue)
            cbPoss = o >= getCbOre (fst blue)
            bbPoss = (o >= getBbOre (fst blue)) && (c >= getBbClay (fst blue))
            gbPoss = (o >= getGbOre (fst blue)) && (b >= getGbObs (fst blue))
            skeep = (t',o',ob,c',cb,b',bb,g',gb)
            keep = [skeep | time && not (obPoss && cbPoss && bbPoss && gbPoss) && not (waste (fst blue) skeep) && not gbPoss && S.null (S.filter (worse skeep) (snd blue))]
            snob = (t',o'-getObOre (fst blue),ob+1,c',cb,b',bb,g',gb)
            nob = [snob | time && obPoss && not gbPoss && not (waste (fst blue) snob) && S.null (S.filter (worse snob) (snd blue))]
            sncb = (t',o'-getCbOre (fst blue),ob,c',cb+1,b',bb,g',gb)
            ncb = [sncb | time && cbPoss && not gbPoss && not (waste (fst blue) sncb) && S.null (S.filter (worse sncb) (snd blue))]
            snbb = (t',o'-getBbOre (fst blue),ob,c'-getBbClay (fst blue),cb,b',bb+1,g',gb)
            nbb = [snbb | time && bbPoss && not gbPoss && not (waste (fst blue) snbb) && S.null (S.filter (worse snbb) (snd blue))]
            sngb = (t',o'-getGbOre (fst blue),ob,c',cb,b'-getGbObs (fst blue),bb,g',gb+1)
            ngb = [sngb | time && gbPoss && not (waste (fst blue) sngb) && S.null (S.filter (worse sngb) (snd blue))]

worse :: State -> State -> Bool
worse s1@(t,o,ob,c,cb,b,bb,g,gb) s2@(t',o',ob',c',cb',b',bb',g',gb') 
    =   (o <= o') && (ob <= ob') && 
        (c <= c') && (cb <= cb') && 
        (b <= b') && (bb <= bb') && 
        (g <= g') && (gb <= gb')

waste :: Blueprint -> State -> Bool
waste (i, o1, o2, o3, co, o4, bo) (t,o,ob,c,cb,b,bb,g,gb) = tooMuchOre || tooMuchClay || tooMuchObs
    where
        remaining = 24 - t
        maxO = maximum [o1,o2,o3,o4]
        tooMuchOre = ob > maxO
        tooMuchClay = cb > co
        tooMuchObs = bb > bo


main :: IO ()
main = do
    --f <- readFile "./input.txt"
    f <- readFile "./test.txt"

    let blueprints = map parse (lines f)
    let possible = reacheableBFS2 (blueprints!!1, S.empty::S.Set State) S.empty (Q.singleton (0,0,1,0,0,0,0,0,0))
    --let possible = reacheableDFS (blueprints!!1) (0,0,1,0,0,0,0,0,0)
    print $ maximum $ S.map getGeodes possible

    putStrLn "done"

dbg :: Show a => a -> a
dbg a = trace (show a ) a

dbg2 :: (Show a, Show m) => m -> a -> a
dbg2 m a = trace (show m ++ ": " ++ show a ) a 
--dbg2 m a = a 