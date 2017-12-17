

input :: Int
input = 394

buildSpinLock :: Int -> Int -> Int -> Int -> [Int] -> [Int]
buildSpinLock seed pos step maxL spinlock
  | seed == maxL = (take newPos spinlock) ++ (seed : (drop newPos spinlock))
  | otherwise = buildSpinLock (seed + 1) newPos step maxL ((take newPos spinlock) ++ (seed : (drop newPos spinlock)))
  where
    newPos = (mod (pos + step) seed) + 1

findNext :: Int -> [Int] -> Int
findNext _ [] = (-1)
findNext val (x:xs)
  | val == x = head xs
  | otherwise = findNext val xs

partialBuild :: Int -> Int -> Int -> Int -> [Int] -> [Int]
partialBuild seed pos step maxL spinlock
  | (seed == maxL) && (newPos == 1) =  (spinlock!!0) : (seed : (tail spinlock))
  | (seed == maxL) && (newPos /= 1) = spinlock
  | (seed /= maxL) && (newPos == 1) = partialBuild (seed + 1) newPos step maxL ((spinlock!!0) : (seed : []))
  | otherwise = partialBuild (seed + 1) newPos step maxL spinlock
  where
    newPos = (mod (pos + step) seed) + 1

