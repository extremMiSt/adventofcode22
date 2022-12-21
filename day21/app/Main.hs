module Main where

import qualified Data.Map as M
import Data.Map ((!))

data Monkey = N Integer | Op String Char String

parseAll :: [String] -> M.Map [Char] Monkey
parseAll [] = M.empty
parseAll (l:ls) = M.insert s m (parseAll ls)
    where
        (s,m) = parse l

parse :: String -> ([Char], Monkey)
parse l = if length w == 2 then parseN w else parseOp w
    where
        w = words l
        

parseN :: [String] -> ([Char], Monkey)
parseN w = (m1, N n)
    where
        m1 = init (head w)
        n = read (w!!1)

parseOp :: [[Char]] -> ([Char], Monkey)
parseOp w = (m1, Op n1 op n2)
    where 
        m1 = init (head w)
        n1 = w!!1
        op = head (w!!2)
        n2 = w!!3

calc :: M.Map String Monkey -> String ->  Integer
calc map monkey = case map ! monkey of
    N i -> i
    Op m1 op m2 -> case op of
        '+' -> calc map m1 + calc map m2
        '-' -> calc map m1 - calc map m2
        '/' -> calc map m1 `div` calc map m2
        '*' -> calc map m1 * calc map m2

data Dir = LeftB | RightB | None
    deriving (Eq, Show)

hasHuman :: M.Map String Monkey -> String -> Dir
hasHuman map monkey = case map ! monkey of
    N i -> None
    Op m1 op m2 | m1 == "humn" -> LeftB
                | m2 == "humn" -> RightB
                | hasHuman map m1 /= None -> LeftB
                | hasHuman map m2 /= None -> RightB
                | otherwise -> None

equ :: M.Map String Monkey -> String -> String
equ map monkey | monkey == "root" = case map ! monkey of
    N i -> show i
    Op m1 op m2 -> "(" ++ equ map m1 ++ "=" ++ equ map m2 ++ ")"
equ map monkey | monkey == "humn" = "x"
equ map monkey = case map ! monkey of
    N i -> show i
    m@(Op m1 op m2) -> case hasHuman map monkey of 
        None -> show $ calc map monkey
        LeftB -> case op of
            '+' -> "(" ++ equ map m1 ++ "+" ++ show (calc map m2) ++ ")"
            '-' -> "(" ++ equ map m1 ++ "-" ++ show (calc map m2) ++ ")"
            '/' -> "(" ++ equ map m1 ++ "/" ++ show (calc map m2) ++ ")"
            '*' -> "(" ++ equ map m1 ++ "*" ++ show (calc map m2) ++ ")"
        RightB -> case op of
            '+' -> "(" ++ show (calc map m1) ++ "+" ++ equ map m2 ++ ")"
            '-' -> "(" ++ show (calc map m1) ++ "-" ++ equ map m2 ++ ")"
            '/' -> "(" ++ show (calc map m1) ++ "/" ++ equ map m2 ++ ")"
            '*' -> "(" ++ show (calc map m1) ++ "*" ++ equ map m2 ++ ")"

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let map = parseAll (lines f)
    print $ calc map "root"

    print $ equ map "root"
    {-
    one can solve that with for example https://www.mathpapa.com/equation-solver/ or https://quickmath.com/ or ...
    with the result being:
    x=3006709232464
    -}
    putStrLn "done"
