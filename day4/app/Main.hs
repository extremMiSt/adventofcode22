module Main where
import Data.Text (Text, splitOn, pack, unpack)
import Data.List (isInfixOf, intersect)

fromLine :: Text -> ([Integer], [Integer])
fromLine t = ([a..b],[c..d])
    where 
        s   = splitOn (pack ",") t
        r1  = splitOn (pack "-") (head s)
        a   = read $ unpack (head r1)
        b   = read $ unpack (r1!!1)
        r2  = splitOn (pack "-") (s!!1)
        c   = read $ unpack (head r2)
        d   = read $ unpack (r2!!1)

isIncluded :: Eq a => [a] -> [a] -> Bool
isIncluded a b = a `isInfixOf` b || b `isInfixOf` a

task1 :: String -> Integer
task1 s = toInteger.length $ filter (uncurry isIncluded . fromLine . pack) (lines s)

overlaps :: Eq a => [a] -> [a] -> Bool
overlaps a b = not.null $ a `intersect` b

task2 :: String -> Integer
task2 s = toInteger.length $ filter (uncurry overlaps . fromLine . pack) (lines s)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    print $ task1 f
    print $ task1O1 f
    print $ task2 f
    print $ task2O1 f


----O(1) space versions----
fromLineO1 :: Text -> ((Integer,Integer), (Integer,Integer))
fromLineO1 t = ((a,b),(c,d))
    where 
        s   = splitOn (pack ",") t
        r1  = splitOn (pack "-") (head s)
        a   = read $ unpack (head r1)
        b   = read $ unpack (r1!!1)
        r2  = splitOn (pack "-") (s!!1)
        c   = read $ unpack (head r2)
        d   = read $ unpack (r2!!1)

isIncludedO1 :: (Integer, Integer) -> (Integer, Integer) -> Bool
isIncludedO1 a b = (fst a <= fst b && snd a >= snd b) || (fst a >= fst b && snd a <= snd b)

overlapsO1 :: (Integer, Integer) -> (Integer, Integer) -> Bool
overlapsO1 a b = (fst a <= fst b && fst b <= snd a) || (fst b <= fst a && fst a <= snd b)

task1O1 :: String -> Integer
task1O1 s = toInteger.length $ filter (uncurry isIncludedO1 . fromLineO1 . pack) (lines s)

task2O1 :: String -> Integer
task2O1 s = toInteger.length $ filter (uncurry overlapsO1 . fromLineO1 . pack) (lines s)
