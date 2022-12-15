module Main where
import qualified Data.Text as T
import qualified Data.Set as S

type Point = (Integer,Integer)
type Distance = Integer

parse :: String -> (Point,Point)
parse l = ((sx,sy),(bx,by))
    where
        s1 = T.splitOn (T.pack "=") (T.pack l)
        s2 = concatMap (T.splitOn (T.pack ", ")) s1
        s3 = concatMap (T.splitOn (T.pack ":")) s2
        sx = read $ T.unpack (s3!!1)
        sy = read $ T.unpack (s3!!3)
        bx = read $ T.unpack (s3!!5)
        by = read $ T.unpack (s3!!7)

distance :: (Point,Point) -> (Point, Distance)
distance (s@(sx,sy),(bx,by)) = (s, d)
    where
        d = abs (sx - bx) + abs (sy - by)

inReach :: S.Set Point -> (Point, Distance) -> Integer -> S.Set Point
inReach set ((x,y),d) line | abs (y-line) > d = set
inReach set (s@(x,y),d) line | abs (y-line) <= d = insertAll set [(x',line)| x' <- [(x-rem) .. (x+rem)]] 
    where 
        rem = d - abs (y-line)

insertAll :: (Ord a) => S.Set a -> [a] -> S.Set a
insertAll = foldl (flip S.insert)

removeAll :: (Ord a) => S.Set a -> [a] -> S.Set a
removeAll = foldl (flip S.delete)

inReachAll :: S.Set Point ->[(Point, Distance)] -> Integer -> S.Set Point
inReachAll s xs i = foldl (\ s x -> inReach s x i) s xs

perimiter :: (Point, Distance) -> [Point]
perimiter (s@(x,y),d) = [(x',y')| y' <- [(y-d-1) .. (y+d+1)], x' <- [x-(d - abs (y-y'))-1,x+(d - abs (y-y'))+1]]

allPerimiters :: [(Point,Distance)] -> [Point]
allPerimiters = concatMap perimiter

isPossible :: [(Point, Distance)] -> Point -> Bool
isPossible [] p = True
isPossible (((x,y),d):xs) p@(x',y') | (x' >= 0) && (x' <= 4000000) && 
                                    (y' >= 0) && (y' <= 4000000) && 
                                    ((abs(x-x') + abs(y-y')) > d) = isPossible xs p
                                | otherwise = False 

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let p = map parse (lines f)
    let t = map distance p
    print $ S.size (removeAll (inReachAll S.empty t 2000000) (map snd p))

    let bs = head (filter (isPossible t) (allPerimiters t))
    print $ (fst bs*4000000)+snd bs
    undefined
