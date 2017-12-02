-- AdventofCode Day 2, Puzzle 2
-- Checksum
-- Add the result of the only "evenly divisable divsors in each "row" of the
-- "spreadsheet" input
-- Uses functions from Scaffold.hs
-- Uses input from Day2input.hs

-- See http://www.adventofcode.com/2017 for details

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

import Scaffold
import Day2input

iFindDivisors :: Int -> [Int] -> (Int, Int)
iFindDivisors x [] = (0, 0)
iFindDivisors x (y:ys)
	      | mod x y == 0	= (x, y)
	      | mod y x == 0 	= (y, x)
	      | otherwise  	= iFindDivisors x ys

findDivisors :: [Int] -> (Int, Int)
findDivisors [] = (0,0)
findDivisors (x:xs) =
	     let idivs = iFindDivisors x xs in
	     	 if idivs == (0,0) then findDivisors xs else idivs

puzzle2_2 :: [[Int]] -> Int
puzzle2_2 l = foldr1 (+) (map (\(x,y) -> div x y) (map findDivisors l))