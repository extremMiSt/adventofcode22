module Main where
import qualified Data.Text as T
import Data.Text (splitOn)

makeEmptyMap :: Int -> Int -> [[Char]]
makeEmptyMap x y = replicate y (replicate x '.')

drawLine :: [(Int,Int)] -> [[Char]]-> [[Char]]
drawLine [] map = map
drawLine [p,s] map| p == s = drawLine [] (replaceAt map p '#')
drawLine (p:s:points) map | p == s = drawLine (s:points) (replaceAt map p '#')
drawLine (p1@(x1,y1):p2@(x2,y2):points)  map
        | x1 == x2 && y1 < y2 = drawLine ((x1,y1+1):p2:points) (replaceAt map (x1,y1) '#')
        | x1 == x2 && y1 > y2 = drawLine ((x1,y1-1):p2:points) (replaceAt map (x1,y1) '#')
        | x1 < x2 && y1 == y2 = drawLine ((x1+1,y1):p2:points) (replaceAt map (x1,y1) '#')
        | x1 > x2 && y1 == y2 = drawLine ((x1-1,y1):p2:points) (replaceAt map (x1,y1) '#')

replaceAt :: [[a]] -> (Int,Int) -> a -> [[a]]
replaceAt m (x,y) e = take y m ++
                    [take x (m!!y) ++ [e] ++ drop (x+1) (m!!y)] ++
                    drop (y+1)m

line :: T.Text -> [(Int, Int)]
line s = map (tuple. T.splitOn (T.pack ",")) (T.splitOn (T.pack " -> ") s)


tuple :: [T.Text] -> (Int, Int)
tuple [a,b] = (toInteger' a, toInteger' b)

toInteger' :: T.Text -> Int
toInteger' = read . T.unpack

dropSnow :: [[Char]] -> (Int,Int) -> [[Char]]
dropSnow map (x,y) | (y+1) >= length map = map
                    | (map!!(y+1))!!x == '.' = dropSnow map (x,y+1)
                    | (map!!(y+1))!!(x-1) == '.' = dropSnow map (x-1,y+1)
                    | (map!!(y+1))!!(x+1) == '.' = dropSnow map (x+1,y+1)
                    | otherwise = replaceAt map (x,y) 'o'

letItSnow :: Num a => [[Char]] -> a
letItSnow map = if done then 0 else 1+ letItSnow map'
    where 
        map' = dropSnow map (500,0)
        done = map' == map

maxY :: String -> Int
maxY f = maximum $ map snd (concatMap line (T.lines (T.pack f)))

main :: IO ()
main = do
    f <- readFile "./input.txt"
    let map' = makeEmptyMap 520 200
    let lines = map line (T.lines (T.pack f))
    let cmap = foldr drawLine map' lines
    print $ letItSnow cmap

    let map2' = makeEmptyMap 1000 200
    let lines2 = map line (T.lines (T.pack f))
    let cmap2 = foldr drawLine map2' lines
    let cmap2' = drawLine [(0,2+ maxY f), (999,2+ maxY f)] cmap2 
    print $ letItSnow cmap2'
    