module Main where
import qualified Data.Text as T
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)

check :: String -> String -> Bool
check ('[':ls) ('[':rs) = check ls rs
check (']':ls) (']':rs) = check ls rs
check (',':ls) (',':rs) = check ls rs
check l@('[':ls) (a:b:rs) | isDigit a && isDigit b = check l ('[':a:b:']':rs)
                          | isDigit a = check l ('[':a:']':b:rs)
check (a:b:ls) r@('[':rs) | isDigit a && isDigit b = check ('[':a:b:']':ls) r
                          | isDigit a = check ('[':a:']':b:ls) r
check (a:ls) (']':rs) = False
check (']':ls) (a:rs) = True
check (a1:b1:ls) (a2:b2:rs) | isDigit a1 && isDigit b1 && isDigit a2 && isDigit b2 = check ls rs --10 vs 10
check (a1:b1:ls) (a2:b2:rs) | isDigit a1 && isDigit b1 && isDigit a2 = False --10 vs x
check (a1:b1:ls) (a2:b2:rs) | isDigit a1 && isDigit a2 && isDigit b2 = True -- x vs 10
check (a1:ls) (a2:rs) | isDigit a1 && isDigit a2 && ((read [a1]::Integer) == read [a2]) = check ls rs --y vs x
                      | isDigit a1 && isDigit a2 = (read [a1]::Integer) < read [a2] --y vs x
check [] [] = True

checkP :: [String] -> Bool
checkP [a,b] = check a b

isDigit :: Char -> Bool
isDigit e = e `elem` ['1','2','3','4','5','6','7','8','9','0']

sumTrueI :: [Bool] -> Integer
sumTrueI l = sum $ map snd (filter fst (zip l [1..]))

task1 :: String -> Integer
task1 f = sumTrueI $ map (checkP.lines.T.unpack) (T.splitOn (T.pack "\n\n") (T.pack f))

task2 :: String -> Int
task2 f = (1 + fromJust (elemIndex "[[2]]" fixed)) * (1 + fromJust (elemIndex "[[6]]" fixed))
    where
        fixed = sortBy (\x y -> if check x y then LT else GT) (filter (""/=) ("[[6]]":"[[2]]":lines f))

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    print $ task1 f
    print $ task2 f

