import Day12Input

testinput :: [(Int, [Int])]
testinput = [(0,[2]), (1,[1]), (2,[0,3,4]), (3,[2,4]), (4,[2,3,6]), (5, [6]), (6,[4,5])]

getProc :: [(Int, [Int])] -> Int -> (Int, [Int])
getProc [] _ = ((-1), [])
getProc ((procID, comms):processes) findID
  | procID == findID = (procID, comms)
  | otherwise = getProc processes findID

addUnique :: Int -> [Int] -> [Int]
addUnique x [] = [x]
addUnique x y
  | elem x y = y
  | otherwise = x:y



getGroup :: [(Int, [Int])] -> [Int] -> [Int] -> [Int]
getGroup notes [] group = group
getGroup notes (process:processes) group
  | elem process group = getGroup notes processes (addUnique process group)
  | otherwise = getGroup notes (processes ++ comms) (addUnique process group)
  where (p, comms) = getProc notes process

getGroups :: [(Int,[Int])] -> [(Int, [Int])] -> [[Int]] -> [[Int]]
getGroups _ [] groups = groups
getGroups whole ((pid, comms):processes) groups
  | groups == [] = getGroups whole processes ((getGroup whole [pid] []):groups) 
  | foldr1 (||) (map (elem pid) groups) = getGroups whole processes groups
  | otherwise = getGroups whole processes ((getGroup whole [pid] []):groups) 
