-- AdventofCode Day 1, Puzzle 2
-- Inverse Capture
-- Add numbers from a sequence but only where digit n = n+(n/2) and the list wraps
-- Uses functions from scaffold.hs

-- See http://www.adventofcode.com/2017 for details

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

import Scaffold

inverseCaptcha :: [Int] -> Int -> Int
inverseCaptcha captcha offset =
	let aptchac = rotateList offset captcha in
	    foldr1 (+) [ m | (m, n) <- zip captcha aptchac , m == n ]

puzzle1_2 :: [Char] -> Int
puzzle1_2 l =
	  inverseCaptcha (stringToNumbers l) (round (fromIntegral (length l) / 2))