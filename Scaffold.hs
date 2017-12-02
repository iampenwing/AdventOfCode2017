-- Useful Functions for Advent of Code 2017

-- See http://www.adventofcode.com/2017 for details of the challenges

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk
-- @penwing // @penwing@cybre.space

module Scaffold where

       charIntOffset :: Int
       charIntOffset = fromEnum '0'

       stringToNumbers :: [Char] -> [Int]
       stringToNumbers l = map (\x -> x - charIntOffset) (map fromEnum l)

       rotateList :: Int -> [Int] -> [Int]
       rotateList 0 l = l
       rotateList n (x:xs) = rotateList (n - 1) (xs ++ [x])
