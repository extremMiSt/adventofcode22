module Main where
import qualified Data.Map as M
import Data.Map ((!))
import Data.List (delete)
import Control.Monad (guard)

type Name = String
type Flow = Integer
data Node = Node Name Flow [Name]
    deriving (Eq, Show)
type Net = M.Map String Node

getName :: Node -> Name
getName (Node n _ _) = n

getFlow :: Node -> Integer
getFlow (Node _ f _) = f

getNext :: Node -> [Name]
getNext(Node _ _ n) = n

parse :: String -> Node
parse l = Node (w!!1) flow names
    where 
        w = words l
        flow = read $ init (drop 5 (w!!4))
        names = map (take 2) (drop 9 w)

readNet :: String -> Net 
readNet f = net M.empty l
    where
        l = lines f
        net n [] = n 
        net n (x:xs) =net (M.insert (getName p) p n) xs
            where
                p = parse x

allPaths :: Net -> Integer -> Node -> Node -> [[Node]]
allPaths net t s p= do
    if t >= 30
        then
            return []
        else do
            n <- getNext s
            let node = net ! n
            if getFlow s > 0
                then do
                    path <- allPaths net (t+1) node s
                    return (s : path)
                else do
                    path <- allPaths net (t+2) node s
                    return (s : path)


calcFlow :: Net -> Integer -> [Node] -> Node -> Node -> Integer -> Integer
calcFlow net time open cur prev flow | time >= 30 = flow
calcFlow net time open cur prev flow = case getNext cur of 
    [a] -> if cur `elem` open 
        then
            calcFlow net (time+1) open (net ! a) cur (flow + flowOf open)
        else
            max
                (calcFlow net (time+2) (cur:open) (net ! a) cur (flow+(2*flowOf open)))
                (calcFlow net (time+1) open (net ! a) cur (flow+(1*flowOf open)))
    l -> if (cur `elem` open) || (getFlow cur == 0)
        then
            maximum (map (\e -> calcFlow net (time+1) open (net ! e) cur (flow+flowOf open)) (delete (getName prev) l))
        else 
            max
                (maximum (map (\e -> calcFlow net (time+2) (cur:open) (net ! e) cur (flow+(2*flowOf open))) l))
                (maximum (map (\e -> calcFlow net (time+1) open (net ! e) cur (flow+flowOf open)) (delete (getName prev) l)))

flowOf :: [Node] -> Integer
flowOf = foldr ((+) . getFlow) 0

main :: IO ()
main = do
    --f <- readFile "./input.txt"
    f <- readFile "./test.txt"
    let n = readNet f
    --let v = calcFlow n 0 [] (n!"AA") (Node "ZZ" 0 []) 0
    print $ length $ (allPaths n 0 (n!"AA") (Node "ZZ" 0 []))
    undefined
