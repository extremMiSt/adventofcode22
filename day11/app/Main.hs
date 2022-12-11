{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.Text as T
import Data.List (sort)
       
data Monkey = Monkey {mid::Integer, items::[Integer], throws::Integer, worryF::Integer->Integer, nextF::Integer->Integer}

readMonkey :: T.Text -> Monkey
readMonkey t = Monkey id items 0 worryF nextF    where
        l = T.lines t
        id = read $ T.unpack (T.dropEnd 1 (T.drop 7 (l!!0)))
        items = map (read . T.unpack) (T.splitOn (T.pack ", ") (T.drop 18 (l!!1)))
        worryF x = wF x (wV x)
        worryL = T.words (l!!2)
        wF = case T.unpack (worryL!!4) of
            "+" -> (+)
            "*" -> (*)
        wV x = case T.unpack (worryL!!5) of
            "old" -> x
            a -> read a
        nextF x = if x `mod` nD == 0 then mA else mB
        nD = read(T.unpack(last(T.words (l!!3))))
        mA = read(T.unpack(last(T.words (l!!4))))
        mB = read(T.unpack(last(T.words (l!!5))))

monkeyThrow :: [Monkey] -> Monkey -> [Monkey] 
monkeyThrow [] monkey = []
monkeyThrow (m:monkeys) monkey | mid m == mid monkey = Monkey (mid m) (tail (items m)) (throws m + 1) (worryF m) (nextF m) : monkeyThrow monkeys monkey
                               | mid m == next = Monkey (mid m) (items m ++ [obj]) (throws m) (worryF m) (nextF m) : monkeyThrow monkeys monkey
                               | otherwise = m: monkeyThrow monkeys monkey
            where 
                obj = worryF monkey (head (items monkey)) `div` 3 
                next = nextF monkey obj

monkeyThrowAll :: [Monkey] -> Monkey -> [Monkey]
monkeyThrowAll monkeys monkey = case items monkey of
    [] -> monkeys
    (i:is) -> monkeyThrowAll 
        (monkeyThrow monkeys monkey) 
        (Monkey (mid monkey) (tail (items monkey)) (throws monkey)(worryF monkey) (nextF monkey))

mround :: [Monkey] -> Int -> [Monkey]
mround monkeys i | i < length monkeys = mround (monkeyThrowAll monkeys (monkeys!!i)) (i+1)
                 | otherwise = monkeys

rounds :: [Monkey] -> [a] -> [Monkey]
rounds m [] = m
rounds !m (x:xs) = rounds (mround m 0) xs

monkeyThrow' :: [Monkey] -> Monkey -> [Monkey] 
monkeyThrow' [] monkey = []
monkeyThrow' (m:monkeys) monkey | mid m == mid monkey = Monkey (mid m) (tail (items m)) (throws m + 1) (worryF m) (nextF m) : monkeyThrow' monkeys monkey
                               | mid m == next = Monkey (mid m) (items m ++ [obj]) (throws m) (worryF m) (nextF m) : monkeyThrow' monkeys monkey
                               | otherwise = m: monkeyThrow' monkeys monkey
            where 
                obj = worryF monkey (head (items monkey)) `mod` (13*7*3*19*5*2*11*17) -- I could try and parse this fro mthe file, but no
                next = nextF monkey obj

monkeyThrowAll' :: [Monkey] -> Monkey -> [Monkey]
monkeyThrowAll' monkeys monkey = case items monkey of
    [] -> monkeys
    (i:is) -> monkeyThrowAll'
        (monkeyThrow' monkeys monkey) 
        (Monkey (mid monkey) (tail (items monkey)) (throws monkey)(worryF monkey) (nextF monkey))

mround' :: [Monkey] -> Int -> [Monkey]
mround' monkeys i | i < length monkeys = mround' (monkeyThrowAll' monkeys (monkeys!!i)) (i+1)
                 | otherwise = monkeys

rounds' :: [Monkey] -> [a] -> [Monkey]
rounds' m [] = m
rounds' !m (x:xs) = rounds' (mround' m 0) xs

main :: IO ()
main = do 
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let f' = T.splitOn (T.pack "\n\n") (T.pack f)
    let m = map readMonkey f'
    print $ product $ take 2 $ reverse . sort $ map  throws (rounds m [1..20])
    print $ product $ take 2 $ reverse . sort $ map  throws (rounds' m [1..10000])
