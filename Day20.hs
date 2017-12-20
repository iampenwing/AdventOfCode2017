import Day20Input


updateBuffer :: [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))] -> [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))]
updateBuffer [] = []
updateBuffer (particle:particles) = (((px+vx+ax),(py+vy+ay),(pz+vz+az)), ((vx+ax), (vy+ay), (vz+az)), (ax, ay, az)) : (updateBuffer particles)
  where
    ((px,py,pz),(vx,vy,vz),(ax,ay,az)) = particle

removeDuplicateParticles :: [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))] -> [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))]
removeDuplicateParticles [] = []
removeDuplicateParticles ((p,v,a):particles)
  | areDuplicatePos particles p = removeDuplicateParticles (removeDuplicateParticles2 particles p)
  | otherwise = ((p,v,a):(removeDuplicateParticles particles))
               
areDuplicatePos :: [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))] -> (Int, Int, Int) -> Bool
areDuplicatePos [] _ = False
areDuplicatePos ((p,_,_):particles) pos = (p==pos) || areDuplicatePos particles pos

removeDuplicateParticles2 :: [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))] -> (Int, Int, Int) -> [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))]
removeDuplicateParticles2 [] _ = []
removeDuplicateParticles2 ((p, v, a):particles) remove
  | (p==remove) = removeDuplicateParticles2 particles remove
  | otherwise = (p,v,a):(removeDuplicateParticles2 (particles) remove)


manhatten :: (Int, Int, Int) -> Int
manhatten (x, y, z) = ((abs x)+(abs y)+(abs z))

manhatten2 :: ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int)) -> (Int, Int, Int)
manhatten2 (p, v, a) = ((manhatten p), (manhatten v), (manhatten a))

updateBuffer2 :: [(Int, Int, Int)] -> [(Int, Int, Int)]
updateBuffer2 [] = []
updateBuffer2 ((p,v,a):particles) = ((p+v+a), (v+a), a):(updateBuffer2 particles)

findClosest2 :: [(Int, Int, Int)] -> (Int, Int) -> Int -> (Int, Int)
findClosest2 [] best _ = best
findClosest2 ((p, _, _):particles) (bestPos, bestVal) pos
  | abs p < bestVal = findClosest2 particles (pos, p) (pos+1)
  | otherwise = findClosest2 particles (bestPos, bestVal) (pos + 1)

findClosest3 :: [(Int, Int, Int)] -> (Int, Int)
findClosest3 ((p,v,a):particles) = findClosest2 particles (0, p) 1

findClosest :: [Int] -> Int -> Int -> Int -> (Int, Int)
findClosest [] b n _ = (n, b)
findClosest (x:xs) best n c
  | (abs x) < best = findClosest xs (abs x) c (c+1)
  | otherwise = findClosest xs best n (c+1)

doRecurse :: [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))] -> Int -> [(Int,Int)]
doRecurse _ 0 = []
doRecurse buffer count = (findClosest (map manhatten (map (\(p,v,a) -> p) buffer)) 40000000 0 0) : (doRecurse (updateBuffer buffer) (count - 1))

doRecurse2 :: [(Int, Int, Int)] -> Int -> [(Int, Int)]
doRecurse2 _ 0 = []
doRecurse2 buffer count = (findClosest3 buffer) : (doRecurse2 (updateBuffer2 buffer) (count - 1))
            
smallestAcc :: [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))] -> Int -> Int -> Int -> Int
smallestAcc [] bestPos _ _ = bestPos
smallestAcc ((_,_,(ax,ay,az)):particles) bestPos bestVal current
  | acc < bestVal = smallestAcc particles current acc (current + 1)
  | otherwise = smallestAcc particles bestPos bestVal (current + 1)
  where
    acc = (abs ax) + (abs ay) + (abs az)

findSmallestAcc :: [(Int, Int, Int)] -> [(Int, Int)] -> Int -> [(Int, Int)]
findSmallestAcc [] smallest _ = smallest
findSmallestAcc ((_,_,a):particles) ((pos, val):rest) count
  | a < val = findSmallestAcc particles [(count, a)] (count + 1)
  | a > val = findSmallestAcc particles ((pos, val):rest) (count + 1)
  | otherwise = findSmallestAcc particles ((count, a):((pos, val):rest)) (count + 1)

findSmallestVel2 :: [(Int, Int, Int)] -> [(Int, Int)]
findSmallestVel2 ((p,v,a):particles) = findSmallestVel particles [(0, v)] 1

findSmallestVel :: [(Int, Int, Int)] -> [(Int, Int)] -> Int -> [(Int, Int)]
findSmallestVel [] smallest _ = smallest
findSmallestVel ((_,v,_):particles) ((pos, val):rest) count
  | v < val = findSmallestAcc particles [(count, v)] (count + 1)
  | v > val = findSmallestAcc particles ((pos, val):rest) (count + 1)
  | otherwise = findSmallestAcc particles ((count, v):((pos, val):rest)) (count + 1)

findSmallestAcc2 :: [(Int, Int, Int)] -> [(Int, Int)]
findSmallestAcc2 ((p,v,a):particles) = findSmallestAcc particles [(0, a)] 1

extractFrom :: [(Int, Int, Int)] -> [Int] -> [(Int, Int, Int)]
extractFrom buffer [] = []
extractFrom buffer (pos:poss) = (buffer!!pos):(extractFrom buffer poss)

countIn :: Int -> [Int] -> Int -> Int
countIn _ [] count = count
countIn val (x:xs) count
  | val == x = countIn val xs (count + 1)
  | otherwise = countIn val xs count

  
doPart2 :: [((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))] -> Int -> Int -> Int -> Int
doPart2 buffer cLen cStability stab
  | cStability > stab = cLen
  | cLen == newLen = doPart2 (updateBuffer newBuffer) cLen (cStability + 1) stab
  | cLen /= newLen = doPart2 (updateBuffer newBuffer) newLen 0 stab
  where
    newBuffer = removeDuplicateParticles buffer
    newLen = length newBuffer
    
