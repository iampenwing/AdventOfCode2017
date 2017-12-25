module Day25 where

instructions  :: [(Char, Int, Int, Bool, Char)]
instructions = [('A', 0, 1, True, 'B'),
         ('A', 1, 0, False, 'C'),
         ('B', 0, 1, False, 'A'),
         ('B', 1, 1, True, 'D'),
         ('C', 0, 0, False, 'B'),
         ('C', 1, 0, False, 'E'),
         ('D', 0, 1, True, 'A'),
         ('D', 1, 0, True, 'B'),
         ('E', 0, 1, False, 'F'),
         ('E', 1, 1, True, 'C'),
         ('F', 0, 1, True, 'D'),
         ('F', 1, 1, True, 'A')]

diagnostic :: Int
diagnostic = 12667664

nextState :: [Int] -> Char -> Int -> (Char, Int, [Int])
nextState tape state pos = (newState, newPos, newTape)
  where
    val = getVal tape pos
    (newVal, direction, newState) = getOutput instructions state val
    newPos = if (direction) then (pos + 1) else (pos - 1)
    newTape = setVal tape pos

getVal :: [Int] -> Int -> Int
getVal [] _ = 0
getVal (x:xs) pos
  | x == pos = 1
  | otherwise = getVal xs pos

setVal :: [Int] -> Int -> [Int]
setVal [] pos
  | pos==1 = [pos]
  | otherwise = []
setVal (p:xs) pos 
  | (p==pos) = xs
  | otherwise = p:(setVal xs pos)

getOutput :: [(Char, Int, Int, Bool, Char)] -> Char -> Int -> (Int, Bool, Char)
getOutput [] _ _ = (0, True, 'X')
getOutput (inst:insts) state val 
  | (state == s) && (val == v) = (o, d, n)
  | otherwise = getOutput insts state val
    where
      (s, v, o, d, n) = inst

nextSteps :: [Int] -> Char -> Int -> Int -> (Char, Int, [Int])
nextSteps tape state pos 0 = (state, pos, tape)
nextSteps tape state pos count = nextSteps newTape newState newPos (count - 1)
  where
    (newState, newPos, newTape) = nextState tape state pos 

part1 :: Int
part1 = length mytape
  where
    (_, _, mytape) = nextSteps [] 'A' 0 diagnostic

         
