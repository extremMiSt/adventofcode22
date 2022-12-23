module Main where
import qualified Data.Set as S
import Data.List ((\\))

type Pos = (Integer, Integer)
data Dir = N | S | W | E

parse :: [Char] -> Integer -> Integer -> S.Set Pos
parse [] _ _ = S.empty
parse ('\n':xs) x y = parse xs 0 (y+1)
parse ('#':xs) x y = S.insert (x,y) (parse xs (x+1) y)
parse (_:xs) x y = parse xs (x+1) y

consider :: S.Set Pos -> [Dir] -> Pos -> (Pos,Pos)
consider set [] p = (p,p)
consider set (d:ds) p@(x,y) = case d of
    _ | S.notMember (x,y-1) set && S.notMember (x+1,y-1) set && S.notMember (x-1,y-1) set && S.notMember (x-1,y) set && 
        S.notMember (x,y+1) set && S.notMember (x+1,y+1) set && S.notMember (x-1,y+1) set && S.notMember (x+1,y) set
            -> (p,p)
    N | S.notMember (x,y-1) set && S.notMember (x+1,y-1) set && S.notMember (x-1,y-1) set -> (p,(x,y-1))
    S | S.notMember (x,y+1) set && S.notMember (x+1,y+1) set && S.notMember (x-1,y+1) set -> (p,(x,y+1))
    W | S.notMember (x-1,y) set && S.notMember (x-1,y+1) set && S.notMember (x-1,y-1) set -> (p,(x-1,y))
    E | S.notMember (x+1,y) set && S.notMember (x+1,y+1) set && S.notMember (x+1,y-1) set -> (p,(x+1,y))
    _ -> consider set ds p

considerAll :: S.Set Pos -> [Dir] -> S.Set (Pos,Pos)
considerAll set d = S.map (consider set d) set

move :: S.Set (Pos,Pos) -> S.Set Pos
move set = S.map snd can `S.union` S.map fst cant
    where
        collisions = S.fromList (map snd (S.toList set) \\ S.toList (S.map snd set))
        (cant,can) = S.partition (\a -> snd a `S.member` collisions) set 

doRound :: S.Set Pos -> [Dir] -> S.Set Pos
doRound set d = move considered
    where 
        considered = considerAll set d

doRounds :: S.Set Pos -> [Dir] -> Integer -> S.Set Pos
doRounds set d 0 = set
doRounds set d n = doRounds (doRound set d) (rotate d) (n-1)

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

box :: S.Set (Pos) -> ((Integer,Integer),(Integer,Integer))
box set = ((minx,maxx),(miny,maxy))
    where 
        minx = S.findMin (S.map fst set)
        maxx = S.findMax (S.map fst set)
        miny = S.findMin (S.map snd set)
        maxy = S.findMax (S.map snd set)

doRounds2 :: S.Set Pos -> [Dir] -> Integer -> Integer
doRounds2 set d n = if next == set then n else doRounds2 next (rotate d) (n+1)
    where 
        next = doRound set d

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    --f <- readFile "./smallTest.txt"
    let elvesInit = parse f 0 0
    let r10 = doRounds elvesInit [N,S,W,E] 10
    let ((minx,maxx),(miny,maxy)) = box (r10)
    let total = (maxx-minx+1)*(maxy-miny+1)
    print $ total - (fromIntegral (S.size r10))
    print $ doRounds2 elvesInit [N,S,W,E] 1

    putStrLn "done"
