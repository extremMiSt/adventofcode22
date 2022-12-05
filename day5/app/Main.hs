module Main where
import Data.List (transpose)
import Data.Char (isSpace)

takeTillEmptyLine :: [String] -> [String]
takeTillEmptyLine (x:xs) = case x of
    ""  ->  []
    a   -> x : takeTillEmptyLine xs

makeTower :: [String] -> Int -> [String]
makeTower s i   | i < length s  = (init.strip) (s!!i) : makeTower s (i+4)
                | otherwise     = []

strip :: String -> String
strip = dropWhile isSpace

cmds :: String -> (Integer, Integer, Integer)
cmds s = (a, f, t)
    where
        w = words s
        a = read (w!!1)
        f = read (w!!3) - 1 
        t = read (w!!5) - 1

applyCmd :: (Integer,Integer,Integer) -> [String]-> [String]
applyCmd (a,f,t) s = applySaved s (a',f,t) 0 (reverse (take a' (s!!f')))
    where 
        applySaved (s:ss) (a,f,t) c tmp     | c == f  = drop a s : applySaved ss (a,f,t) (c+1) tmp
                                            | c == t  = (tmp ++ s) : applySaved ss (a,f,t) (c+1) tmp
                                            | otherwise = s : applySaved ss (a,f,t) (c+1) tmp
        applySaved [] _ _ _ = []

        a' = fromInteger a
        f' = fromInteger f

applyCmd2 :: (Integer,Integer,Integer) -> [String]-> [String]
applyCmd2 (a,f,t) s = applySaved s (a',f,t) 0 (take a' (s!!f'))
    where 
        applySaved (s:ss) (a,f,t) c tmp     | c == f  = drop a s : applySaved ss (a,f,t) (c+1) tmp
                                            | c == t  = (tmp ++ s) : applySaved ss (a,f,t) (c+1) tmp
                                            | otherwise = s : applySaved ss (a,f,t) (c+1) tmp
        applySaved [] _ _ _ = []

        a' = fromInteger a
        f' = fromInteger f

task1 :: Foldable t => [[Char]] -> t (Integer, Integer, Integer) -> [Char]
task1 tower cmd=  map head (foldl (flip applyCmd) tower cmd)

task2 :: Foldable t => [[Char]] -> t (Integer, Integer, Integer) -> [Char]
task2 tower cmd=  map head (foldl (flip applyCmd2) tower cmd)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "test.txt"
    let tower = makeTower (transpose $ takeTillEmptyLine (lines f)) 1
    let cmd = map cmds (drop (1 + length (takeTillEmptyLine (lines f))) (lines f))

    print $ task1 tower cmd
    print $ task2 tower cmd

