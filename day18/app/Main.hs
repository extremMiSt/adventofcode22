{-# LANGUAGE InstanceSigs #-}
module Main where
import qualified Data.Set as S

type Cube = (Integer,Integer,Integer)
data Side = Side Cube Cube
    deriving (Show, Ord)

instance Eq Side where
  (==) :: Side -> Side -> Bool
  Side a b == Side c d = (a==c && b==d) || (a==d && b==c)

sides :: Cube -> [Side]
sides o@(a,b,c) = [Side o (a+1,b,c), Side o (a-1,b,c),
                              Side o (a,b+1,c), Side o (a,b-1,c),
                              Side o (a,b,c+1), Side o (a,b,c-1)]

parse :: [Char] -> Cube
parse s = triplet $ read ("["++s++"]")

triplet :: [Integer] -> Cube
triplet [a,b,c] = (a,b,c)

rdups :: Eq a => [a] -> [a]
rdups [] = []
rdups (x:xs) = if c then rdups (filter (/=x) xs) else x: rdups xs
    where
        c = x `elem` xs

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let faces = rdups $ concatMap (sides . parse) (lines f)
    print $ length faces

    putStrLn "done"
