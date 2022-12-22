module Main where
import Data.Char (isDigit, ord)
import qualified Data.Map as M
import Debug.Trace (trace)
import GHC.Stack (HasCallStack)

(%) :: HasCallStack => [a] -> Int -> a
map % k = if ((k-1) < 0) then undefined else map !! (k-1)

findStart :: HasCallStack => [[Char]] -> Int -> (Int, Int)
findStart map x = if (map%1)%x == '.'
    then (x, 1)
    else findStart map (x+1)

data Movement = Walk Integer | Turn Turn
    deriving (Show,Eq)
data Turn = TRight | TLeft
    deriving (Show,Eq, Enum)
data Direction = DRight | DDown | DLeft | DUp
    deriving (Show,Eq,Ord,Enum)

parse :: HasCallStack => String -> [Movement]
parse [] = []
parse (s:t:u:ss) | isDigit s && isDigit t && isDigit u = Walk (read [s,t,u]) : parse ss
                 | isDigit s && isDigit t = Walk (read [s,t]) : parse (u:ss)
                 | isDigit s = Walk (read [s]) : parse (t:u:ss)
                 | otherwise = Turn (if s == 'R' then TRight else TLeft) : parse (t:u:ss)
parse (s:t:ss) | isDigit s && isDigit t = Walk (read [s,t]) : parse ss
               | isDigit s = Walk (read [s]) : parse (t:ss)
               | otherwise = Turn (if s == 'R' then TRight else TLeft) : parse (t:ss)
parse (s:ss) | isDigit s = Walk (read [s]) : parse ss
             | otherwise = Turn (if s == 'R' then TRight else TLeft) : parse ss

fixMap :: [String] -> [String]
fixMap m = helper m
    where
        max = maximum (map length m)
        helper [] = []
        helper (l:ls) = (l ++ replicate (max - length l) ' ') : helper ls

turn :: Direction -> Turn -> Direction
turn DUp TRight = DRight
turn d TRight = succ d
turn DRight TLeft = DUp
turn d TLeft = pred d

type Length = Integer
type Pos = (Int,Int)

walk :: [String] -> Direction -> Length -> Pos -> Pos
walk map dir 0 pos = pos
walk map dir n pos = case next map dir pos of
    Nothing -> pos
    Just p -> walk map dir (n-1) p

next :: [String] -> Direction -> Pos -> Maybe Pos
next map dir (x,y) = case dir of 
    DUp | ((y-1) >= 1) && ((map%(y-1))%x) == '.' -> Just (x, y-1)
        | ((y-1) >= 1) && ((map%(y-1))%x) == '#' -> Nothing
        | ((y-1) >= 1) && ((map%(y-1))%x) == ' ' -> next map DUp (x, y-1)
        | (y-1) < 1 -> next map DUp (x, length map+1)
    DRight | ((x+1) <= length (map%y)) && ((map%y)%(x+1)) == '.' -> Just (x+1, y)
        | ((x+1) <= length (map%y)) && ((map%y)%(x+1)) == '#' -> Nothing
        | ((x+1) <= length (map%y)) && ((map%y)%(x+1)) == ' ' -> next map DRight (x+1, y)
        | (x+1) > length (map%y) -> next map DRight (0, y)
    DDown | ((y+1) <= length map) && ((map%(y+1))%x) == '.' -> Just (x, y+1)
        | ((y+1) <= length map) && ((map%(y+1))%x) == '#' -> Nothing
        | ((y+1) <= length map) && ((map%(y+1))%x) == ' ' -> next map DDown (x, y+1)
        | (y+1) > length map -> next map DDown (x, 0)
    DLeft | ((x-1) >= 1) && ((map%y)%(x-1)) == '.' -> Just (x-1, y)
        | ((x-1) >= 1) && ((map%y)%(x-1)) == '#' -> Nothing
        | ((x-1) >= 1) && ((map%y)%(x-1)) == ' ' -> next map DLeft (x-1, y)
        | (x-1) < 1 -> next map DLeft (length (map%y)+1, y)

move :: [String] -> Direction -> Pos -> Movement -> (Direction, Pos)
move map dir pos mvmnt = case mvmnt of
    Walk n -> (dir, walk map dir n pos)
    Turn t -> (turn dir t, pos)

moveAll :: [String] -> Direction -> Pos -> [Movement] -> (Direction, Pos)
moveAll map dir pos [] = (dir, pos)
moveAll map dir pos (m:ms) = moveAll map dir' pos' ms
    where 
        (dir', pos') = move map dir pos m

score :: (Direction, Pos) -> Int
score (d, (x,y)) = (1000*y) + (4*x) + fromEnum d

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"

    let map = fixMap $ init $ init (lines f)
    let path = parse (last (lines f))
    let spos = findStart map 1

    let fpos = moveAll map DRight spos path
    print $ score fpos

    let fpos2 = moveAll2 map DRight spos path
    print $ score fpos2

    putStrLn "done"

mapEdge :: HasCallStack => M.Map (Direction, Int, Int) (Direction, Int, Int)
mapEdge = up1 `M.union` right1 `M.union` down1 `M.union` 
        up2 `M.union` left2 `M.union` 
        left3 `M.union` right3 `M.union`
        right4 `M.union` down4 `M.union`
        up5 `M.union` left5 `M.union`
        left6 `M.union` down6 `M.union` right6
    where
        up1 = M.fromList (zip [(DUp,x,0) | x <- [101..150]] [(DUp,x,200)| x <- [1..50]])
        right1 = M.fromList (zip [(DRight,151,x) | x <- [1..50]] (reverse [(DLeft,100,x)| x <- [101..150]]))
        down1 = M.fromList (zip [(DDown,x,51) | x <- [101..150]] [(DRight,100,x)| x <- [51..100]])
        up2 = M.fromList (zip [(DUp,x,0) | x <- [51..100]] [(DRight,1,x)| x <- [151..200]])
        left2 = M.fromList (zip [(DLeft,50,x) | x <- [1..50]] (reverse [(DRight,1,x)| x <- [101..150]]))
        left3 = M.fromList (zip [(DLeft,50,x) | x <- [51..100]] [(DUp,x,101)| x <- [1..50]])
        right3 = M.fromList (zip [(DRight,101,x) | x <- [51..100]] [(DUp,x,50)| x <- [101..150]])
        right4 = M.fromList (zip [(DRight,101,x) | x <- [101..150]] (reverse [(DLeft,150,x)| x <- [1..50]]))
        down4 = M.fromList (zip [(DDown,x,151) | x <- [51..100]] (reverse [(DLeft,50,x)| x <- [1..151]]))
        up5 = M.fromList (zip [(DUp,x,100) | x <- [1..50]] [(DRight,51,x)| x <- [51..100]])
        left5 = M.fromList (zip [(DLeft,0,x) | x <- [101..150]] (reverse [(DRight,51,x)| x <- [1..50]]))
        right6 = M.fromList (zip [(DRight,51,x) | x <- [151..200]] [(DUp,x,150)| x <- [51..100]])
        down6 = M.fromList (zip [(DDown,x,201) | x <- [1..50]] [(DDown,x,1)| x <- [101..150]])
        left6 = M.fromList (zip [(DLeft,0,x) | x <- [151..200]] [(DUp,x,1)| x <- [51..100]])

m :: HasCallStack => (Direction, (Int, Int)) -> (Direction, (Int, Int))
m t@(d,(x,y)) = case mapEdge M.!? (d,x,y) of
    Just (d',x',y') -> (d', (x',y'))
    Nothing -> if (x<1 || y<1) then trace (show (x,y)) undefined else t

next2 :: HasCallStack => [String] -> Direction -> Pos -> Maybe (Direction, Pos)
next2 map dir (x,y) = case dir of 
    DUp | (d,(x',y')) <- m (dir,(x,y-1)), (map%y')%x' == '.' -> Just (d, (x', y'))
        | (d,(x',y')) <- m (dir,(x,y-1)), (map%y')%x' == '#' -> Nothing
    DRight | (d,(x',y')) <- m (dir,(x+1,y)), (map%y')%x' == '.' -> Just (d, (x', y'))
           | (d,(x',y')) <- m (dir,(x+1,y)), (map%y')%x' == '#' -> Nothing
    DDown | (d,(x',y')) <- m (dir,(x,y+1)), (map%y')%x' == '.' -> Just (d, (x', y'))
          | (d,(x',y')) <- m (dir,(x,y+1)), (map%y')%x' == '#' -> Nothing
    DLeft | (d,(x',y')) <- m (dir,(x-1,y)), (map%y')%x' == '.' -> Just (d, (x', y'))
          | (d,(x',y')) <- m (dir,(x-1,y)), (map%y')%x' == '#' -> Nothing
    _ -> trace (show (dir,(x,y)) ++ show (x-1,y) ++ show (m (dir,(x-1,y))) ++ show ((map%why)%ex)) undefined
        where 
            (rich, (ex, why)) = m (dir,(x-1,y))

walk2 :: HasCallStack => [String] -> Direction -> Length -> Pos -> (Direction, Pos)
walk2 map dir 0 pos = (dir,pos)
walk2 map dir n pos = case next2 map dir pos of
    Nothing -> (dir,pos)
    Just (dir',pos') -> walk2 map dir' (n-1) pos'

move2 :: HasCallStack => [String] -> Direction -> Pos -> Movement -> (Direction, Pos)
move2 map dir pos mvmnt = case mvmnt of
    Walk n -> walk2 map dir n pos
    Turn t -> (turn dir t, pos)

moveAll2 :: HasCallStack => [String] -> Direction -> Pos -> [Movement] -> (Direction, Pos)
moveAll2 map dir pos [] = (dir, pos)
moveAll2 map dir pos (m:ms) = moveAll2 map dir' pos' ms
    where 
        (dir', pos') = move2 map dir pos m