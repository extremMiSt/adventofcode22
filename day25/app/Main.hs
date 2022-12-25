module Main where
import Data.Char (chr, ord)

value :: Char -> Integer
value '0' = 0
value '1' = 1
value '2' = 2
value '-' = -1
value '=' = -2

toDec :: String -> Integer
toDec s = h (reverse s)
    where 
        h [] = 0
        h (d:ds) = value d + (5* h ds)

toR5 :: Integer -> [Integer]
toR5 0 = []
toR5 i = fromIntegral m : toR5 d
    where 
        (d,m) = divMod i 5

toSnafu :: Integer -> String
toSnafu i = reverse $ h b5
    where
        b5 = toR5 i
        h :: [Integer] -> String
        h [] = ""
        h (d:ds) = case d of
            5 -> '0' : if null ds then ['1'] else h ((1+head ds):tail ds)
            4 -> '-' : if null ds then ['1'] else h ((1+head ds):tail ds)
            3 -> '=' : if null ds then ['1'] else h ((1+head ds):tail ds)
            2 -> '2' : if null ds then [] else h ds
            1 -> '1' : if null ds then [] else h ds
            0 -> '0' : if null ds then [] else h ds

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"

    putStrLn $ toSnafu $ sum $ map toDec (lines f)

    putStrLn "done"

