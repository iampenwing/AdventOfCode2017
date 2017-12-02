-- AdventofCode Day 1, Puzzle 1
-- Inverse Capture
-- Add numbers from a sequence but only where digit n = n+1 and the list wraps
-- Uses functions from scaffold.hs

-- See http://www.adventofcode.com/2017 for details

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

import Scaffold

inverseCaptcha :: [Int] -> Int
inverseCaptcha captcha =
	let aptchac = rotateList 1 captcha in
	    foldr1 (+) [ m | (m, n) <- zip captcha aptchac , m == n ]

puzzle1_1 :: [Char] -> Int
puzzle1_1 l =
	  inverseCaptcha (stringToNumbers l)


