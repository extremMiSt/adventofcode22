module Main where
import Data.Char (isDigit, ord)

(%) :: [a] -> Int -> a
map % k = map !! (k-1)

findStart :: [[Char]] -> Int -> (Int, Int)
findStart map x = if (map%1)%x == '.'
    then (x, 1)
    else findStart map (x+1)

data Movement = Walk Integer | Turn Turn
    deriving (Show,Eq)
data Turn = TRight | TLeft
    deriving (Show,Eq, Enum)
data Direction = DRight | DDown | DLeft | DUp
    deriving (Show,Eq, Enum)

parse :: String -> [Movement]
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

    putStrLn "done"
