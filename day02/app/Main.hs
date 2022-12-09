module Main where

------------Shape type------------
data Shape = Rock | Paper | Scissor
    deriving (Eq,Show)

shapeFromString :: String -> Shape
shapeFromString "A" = Rock
shapeFromString "X" = Rock
shapeFromString "B" = Paper
shapeFromString "Y" = Paper
shapeFromString "C" = Scissor
shapeFromString "Z" = Scissor

shapeValue :: Shape -> Integer
shapeValue Rock = 1
shapeValue Paper = 2
shapeValue Scissor = 3

shapeFromI :: (Eq a, Num a) => a -> Shape
shapeFromI 1 = Rock
shapeFromI 2 = Paper
shapeFromI 3 = Scissor

------------Result type------------
data Result = Loose | Tie | Win 
    deriving (Eq,Show)

resultFromString :: String -> Result
resultFromString "X" = Loose
resultFromString "Y" = Tie
resultFromString "Z" = Win

resultFromI :: (Eq a, Num a) => a -> Result
resultFromI (-2) = Loose
resultFromI (-1) = Win
resultFromI 0 = Tie
resultFromI 1 = Loose
resultFromI 2 = Win

resultToI :: Num a => Result -> a
resultToI Loose = -1
resultToI Tie = 0
resultToI Win = 1

resultValue :: Num a => Result -> a
resultValue Win = 6
resultValue Tie = 3
resultValue Loose = 0

------------calcing  stuff------------
win :: Shape -> Shape -> Result
win a b = resultFromI (shapeValue a - shapeValue b)

score :: String -> Integer
score s = resultValue(win a b) + shapeValue b
    where 
        a = shapeFromString [head s]
        b = shapeFromString [s!!2]

makeResult :: Shape -> Result -> Shape
makeResult s r = shapeFromI p'
    where 
        p = shapeValue s + resultToI r
        p' = if p == 0 then 3 else if p == 4 then 1 else p

score2 :: [Char] -> Integer
score2 s = resultValue b + shapeValue (makeResult a b)
    where 
        a = shapeFromString [head s]
        b = resultFromString [s!!2]
main :: IO ()
main = do
    f <- readFile "./input.txt"
    let task1 = sum (map score (lines f))
    print task1
    let task2 = sum (map score2 (lines f))
    print task2
    return ()
    