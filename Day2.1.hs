-- AdventofCode Day 2, Puzzle 1
-- Checksum
-- Add the difference between largest and smallest in each "row" of the
-- "spreadsheet" input
-- Uses functions from Scaffold.hs
-- Uses input from Day2input.hs

-- See http://www.adventofcode.com/2017 for details

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

import Scaffold
import Day2input

listMinMax :: [Int] -> (Int, Int)
listMinMax (x:xs) = iListMinMax xs (x, x)

iListMinMax :: [Int] -> (Int, Int) -> (Int, Int)
iListMinMax [] (imin, imax) = (imin, imax)
iListMinMax (x:xs) (imin, imax)
	    | x < imin	 = iListMinMax xs (x, imax)
	    | x > imax	 = iListMinMax xs (imin, x)
	    | otherwise	 = iListMinMax xs (imin, imax)

puzzle2_1 :: [[Int]] -> Int
puzzle2_1 l = foldr1 (+) (map (\(x,y) -> y - x) (map listMinMax l))