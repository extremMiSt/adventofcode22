module Main where
import Data.List.Split (chunksOf)

type Time = Integer
type Strength = Integer
type Cmd = (Time, Strength)
type Log = [(Time,Strength)]

toCmd :: [Char] -> Cmd
toCmd s = case take 4 s of
    "noop" -> (1,0)
    "addx" -> (2,v)
        where 
            v = read (drop 5 s)

perform :: Log -> Time -> Strength -> Cmd -> (Log, Time, Strength)
perform log cycle value (1, chng) = ((cycle+1, value + chng):log, cycle+1, value + chng)
perform log cycle value (a, chng) = perform ((cycle + 1, value):log) (cycle+1) value (a-1, chng)

performAll :: Log -> Time -> Strength -> [Cmd] -> (Log, Time, Strength)
performAll log cycle value [] = (log,cycle,value)
performAll log cycle value (c:cs) = performAll log' cycle' value' cs
    where 
        (log', cycle', value') = perform log cycle value c


sumAt :: Log -> [Int] -> Integer
sumAt l [] = 0
sumAt l (p:ps) = (t*v) + sumAt l ps
    where
        (t,v) = l!!p 

task1 :: [String] -> Integer
task1 s = sumAt (reverse log) [20-1,60-1,100-1,140-1,180-1,220-1]
    where 
        (log, _,_) = performAll [(1,1)] 1 1 (map toCmd s)

draw :: Log -> String
draw [] = ""
draw (l:log) = if abs ((snd l+1) - (fst l `mod` 40)) <= 1 
    then
        '#' : draw log
    else
        ' ' : draw log

task2 :: [String] -> String
task2 s = unlines $ chunksOf 40 (draw (reverse log))
    where 
        (log, _,_) = performAll [(1,1)] 1 1 (map toCmd s)

main :: IO ()
main = do 
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let w = lines f

    print $ task1 w
    putStr $ task2 w
