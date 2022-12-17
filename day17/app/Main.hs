module Main where
import qualified Data.Set as S
import Data.Maybe (fromMaybe, fromJust)
import Debug.Trace (trace)
import Data.List (findIndex, elemIndex, nub)

hline :: Integer -> S.Set (Integer, Integer)
hline i = S.fromList [(2,i),(3,i),(4,i),(5,i)]

cross :: Integer -> S.Set (Integer, Integer)
cross i = S.fromList [(3,i),(2,i+1),(3,i+1),(4,i+1), (3, i+2)]

ell :: Integer -> S.Set (Integer, Integer)
ell i = S.fromList [(2,i),(3,i),(4,i),(4,i+1),(4,i+2)]

vline :: Integer -> S.Set (Integer, Integer)
vline i = S.fromList [(2,i),(2,i+1),(2,i+2),(2,i+3)]

box :: Integer -> S.Set (Integer, Integer)
box i = S.fromList [(2,i),(3,i),(2,i+1),(3,i+1)]

rocks :: [Integer -> S.Set (Integer, Integer)]
rocks = cycle [hline, cross, ell, vline, box]

fall :: S.Set (Integer, Integer) -> Char -> S.Set (Integer, Integer) -> (S.Set (Integer, Integer), Bool)
fall field w shape = (down', inBounds down && S.disjoint down field)
    where
        pushed = S.map (\(x,y) -> if w == '<' then (x-1,y) else (x+1,y)) shape
        pushed' = if inBounds pushed && S.disjoint pushed field then pushed else shape
        down = S.map (\(x,y) -> (x,y-1)) pushed'
        down' = if inBounds down && S.disjoint down field  then down else pushed'

inBounds :: S.Set (Integer, Integer) -> Bool
inBounds set = null (S.filter (\(x,y) -> (x < 0) || (x > 6)) set) && null (S.filter (\(x,y) -> y <= 0) set)

fallDown :: S.Set (Integer, Integer) -> String -> S.Set (Integer, Integer) -> (S.Set (Integer, Integer), String)
fallDown field (w:wind) shape = if bounds then fallDown field wind fallen else (S.union field fallen, wind)
    where 
        (fallen, bounds) = fall field w shape

fallAll :: S.Set (Integer, Integer) -> String -> [Integer -> S.Set (Integer, Integer)] -> S.Set (Integer, Integer)
fallAll field wind [] = field
fallAll field wind (s:shape) = fallAll field' wind' shape
    where
        (field', wind') = fallDown field wind (s ((0 `fromMaybe` S.lookupMax (S.map snd field)) + 4))

towers :: S.Set (Integer, Integer) -> String -> [Integer -> S.Set (Integer, Integer)] -> [S.Set (Integer, Integer)]
towers field wind (s:shape) = field : towers field' wind' shape
    where
        (field', wind') = fallDown field wind (s ((0 `fromMaybe` S.lookupMax (S.map snd field)) + 4))

height :: (Num a1, Ord a1) => S.Set (a2, a1) -> a1
height tower = 0 `fromMaybe` S.lookupMax (S.map snd tower)

isFullLine :: (Ord a, Ord b, Num a) => S.Set (a, b) -> b -> Bool
isFullLine tower l = S.member (0,l) tower && S.member (1,l) tower &&
                        S.member (2,l) tower && S.member (3,l) tower &&
                        S.member (4,l) tower && S.member (5,l) tower &&
                        S.member (6,l) tower

topFull :: (Ord a, Ord t, Num t, Num a) => S.Set (a, t) -> t -> t
topFull tower 0 = 0
topFull tower l = if isFullLine tower l then l else topFull tower (l-1)

towersFast :: [Char] -> Int -> Integer
towersFast f n = height tower1 + (((toInteger n - 124) `div` 1678)*(height (t!!(1678+124)) - height (t!!124)))
    where 
        t = towers S.empty (cycle (init f)) rocks
        tower1 = t!! (124 + ((n - 124) `mod` 1678))

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let tower = towers S.empty (cycle (init f)) rocks
    print $ S.findMax (S.map snd (tower!!2022))

    print $ towersFast f 1000000000000 --tut nicht

allIndex :: (Eq a) => a -> [a] -> [Int]
allIndex elem [] = []
allIndex elem list = first : allIndex elem (drop (first+1) list)
    where
        first = fromJust $ elemIndex elem list 

diff :: Num c => [c] -> [c]
diff list = zipWith (-) (tail list) list

chng :: Eq c => [c] -> [Bool]
chng list = zipWith (/=) (tail list) list