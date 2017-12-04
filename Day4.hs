

import Day4Input

areRepeats :: [String] -> Bool
areRepeats [] = False
areRepeats (x:xs) = iAreRepeats x xs || areRepeats xs

iAreRepeats :: String -> [String] -> Bool
iAreRepeats y [] = False
iAreRepeats y (x:xs) = elem y (x:xs) || iAreRepeats y xs

countFalse :: [Bool] -> Int
countFalse [] = 0
countFalse (False:xs) = 1 + countFalse xs
countFalse (True:xs) = countFalse xs

countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue (True:xs) = 1 + countTrue xs
countTrue (False:xs) = countTrue xs

sortString :: [Char] -> [Char]
sortString [] = []
sortString (x : xs) = iSortString x (sortString xs)

iSortString :: Char -> [Char] -> [Char]
iSortString x [] = [x]
iSortString x (y:ys)
  | x <= y = x : (y:ys)
  | otherwise = y : (iSortString x ys)

