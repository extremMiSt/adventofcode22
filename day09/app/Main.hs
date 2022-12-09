module Main where
import Data.List (nub)

type Position = (Integer, Integer)
type Cmd = (Char, Integer)
type Rope = [Position]

toCmd :: String -> Cmd
toCmd l = (c, d)
    where 
        c = head l
        d = read $ drop 2 l


move :: Rope -> [Position] -> Cmd -> (Rope, [Position])
move r l (_ , 0) = (r , l)
move ((hx,hy):rs) l ('U', d) = move newPos newL ('U', d-1) 
    where
        newPos = moveTail ((hx,hy+1):rs)
        newL = last newPos : l
move ((hx,hy):rs) l ('D', d) =  move newPos newL ('D', d-1) 
    where
        newPos = moveTail ((hx,hy-1):rs)
        newL = last newPos : l 
move ((hx,hy):rs) l ('L', d) = move newPos newL ('L', d-1) 
    where
        newPos = moveTail ((hx-1,hy):rs)
        newL = last newPos : l
move ((hx,hy):rs) l ('R', d) =  move newPos newL ('R', d-1) 
    where
        newPos = moveTail ((hx+1,hy):rs)
        newL = last newPos : l

moveTail :: Rope -> Rope 
moveTail ((hx,hy):(tx,ty):rs)  | max (abs(hx - tx)) (abs(hy-ty)) <= 1 = (hx,hy):(tx,ty):rs 
                                | hx == tx && hy > ty = (hx,hy) : moveTail ((tx,ty+1):rs)
                                | hx == tx && hy < ty = (hx,hy) : moveTail ((tx,ty-1):rs)
                                | hy == ty && hx > tx = (hx,hy) : moveTail ((tx+1,ty):rs)
                                | hy == ty && hx < tx = (hx,hy) : moveTail ((tx-1,ty):rs)
                                | hx > tx && hy > ty = (hx,hy) : moveTail ((tx+1,ty+1):rs)
                                | hx < tx && hy > ty = (hx,hy) : moveTail ((tx-1,ty+1):rs)
                                | hx < tx && hy < ty = (hx,hy) : moveTail ((tx-1,ty-1):rs)
                                | hx > tx && hy < ty = (hx,hy) : moveTail ((tx+1,ty-1):rs)
moveTail a = a

moveAll :: Rope -> [Position] -> [Cmd] -> (Rope, [Position])
moveAll r p (c:cs) = moveAll r' p' cs
    where 
        (r', p') = move r p c
moveAll r p [] = (r,p)

initR :: Int -> Rope
initR x = replicate x (0, 0)

main :: IO ()
main = do
    f <- readFile "./input.txt"
    --f <- readFile "./test.txt"
    let cmds = map toCmd (lines f)
    print $ (length.nub.snd) (moveAll (initR 2) [last (initR 2)] cmds)
    print $ (length.nub.snd) (moveAll (initR 10) [last (initR 10)] cmds)
