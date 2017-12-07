
import Scaffold
import Day6Input

balance :: [Int] -> [Int]
balance l = iBalance v (mod (p+1) (length l)) la
  where
    (v, p) = findHighest l 0 (0,0)
    la = ((take p l) ++ [0] ++ (drop (p+1) l))

iBalance :: Int -> Int -> [Int] -> [Int]
iBalance 0 p l = l
iBalance v p l = iBalance
                   (v - 1)
                   (mod (p + 1) (length l))
                   ((take p l) ++ [(l!!p + 1)] ++ (drop (p + 1) l))


findHighest :: [Int] -> Int -> (Int, Int) -> (Int, Int)
findHighest [] n (val, pos) = (val, pos)
findHighest (x:xs) n (val, pos)
  | x > val   = findHighest xs (n + 1) (x, n)
  | otherwise = findHighest xs (n + 1) (val, pos)

fromList :: Int -> Int -> [Int] -> [Int]
fromList x 0 zs = []
fromList x y zs = drop x (take y zs)

findRepeat :: Int -> [[Int]] -> [Int] -> (Int, [[Int]])
findRepeat count mem l
  | elem balanced mem = (count, (balanced:mem))
  | otherwise = findRepeat (count + 1) (balanced:mem) balanced
  where
    balanced = balance l

getLoopLength :: Int -> [Int] -> [[Int]] -> Int
getLoopLength n i (x:xs)
  | i == x = n
  | otherwise = getLoopLength (n + 1) i xs
    
puzzle1 :: [Int] -> Int
puzzle1 l = count
  where
    (count, mem) = findRepeat 1 [l] l

puzzle2 :: [Int] -> Int
puzzle2 l = getLoopLength 1 loop mem
  where
    (count, (loop:mem)) = findRepeat 1 [l] l
    
