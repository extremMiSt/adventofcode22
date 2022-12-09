module Main where


isVisible :: Int -> Int -> Int -> [[Int]] -> Bool
isVisible x y size map = visLeft (x-1) y || visRight (x+1) y || visUp x (y-1) || visDown x (y+1)
    where
        height = map !! x !! y
        visLeft x y =  if x >= 0 then  (((map !! x) !! y) < height) && visLeft (x-1) y else True
        visRight x y =  if x < size then (((map !! x) !! y) < height) && visRight (x+1) y else  True
        visUp x y =  if y >= 0 then (((map !! x) !! y) < height) && visUp x (y-1) else True
        visDown x y =  if y < size then (((map !! x) !! y) < height) && visDown x (y+1) else True

map2D :: (a -> b) -> [[a]] -> [[b]]
map2D f = map (map f)

sum2D :: Num a => [[a]] -> a
sum2D = sum . map sum

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

indices :: Int -> [[(Int, Int)]]
indices size = splitEvery size [(x,y) | x <- [0..(size-1)], y <- [0..(size-1)]]

task1 :: Num a => [[Char]] -> Int -> a
task1 s l = sum2D (map2D (\(x,y) -> if isVisible x y l ( map2D (read.(:[])) s) then 1 else 0) (indices l))

scenicScore :: Int -> Int -> Int -> [[Int]] -> Int
scenicScore x y size map = scoreLeft (x-1) y * scoreRight (x+1) y * scoreUp x (y-1) * scoreDown x (y+1)
    where
        height = map !! x !! y
        scoreLeft x y =  if x >= 0 then if ((map !! x) !! y) >= height then 1 else 1 + scoreLeft (x-1) y else 0
        scoreRight x y =  if x < size then if ((map !! x) !! y) >= height then 1 else 1 + scoreRight (x+1) y else  0
        scoreUp x y =  if y >= 0  then if ((map !! x) !! y) >= height then 1 else 1 + scoreUp x (y-1) else 0
        scoreDown x y =  if y < size  then if ((map !! x) !! y) >= height then 1 else 1 + scoreDown x (y+1) else 0

max2D :: (Ord a) => [[a]] -> a
max2D = maximum . map maximum

task2 :: [[Char]] -> Int -> Int
task2 s l = max2D (map2D (\(x,y) -> scenicScore x y l (map2D (read.(:[])) s)) (indices l))

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let f' = lines f
    let size = length (head f')
    print $ task1 f' size
    print $ task2 f' size
