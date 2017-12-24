import Day24Input

extractValidPieces :: [(Int,Int)] -> Int -> [(Int,Int)]
extractValidPieces components ports = [(x,y) | (x,y) <- components, ((x==ports) || (y==ports))] 

removeSelectedPiece :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
removeSelectedPiece [] _ = []
removeSelectedPiece (piece:pieces) remPiece
  | piece==remPiece = pieces
  | otherwise = piece:(removeSelectedPiece pieces remPiece)


buildBridges :: [(Int,Int)] -> Int -> [Int]
buildBridges components ports
  | validComponents == [] = [0]
  | otherwise = foldr1 (++) (map (doSomething components ports) validComponents)
  where
    validComponents = extractValidPieces components ports

doSomething :: [(Int, Int)] -> Int -> (Int,Int) -> [Int]
doSomething components ports piece = map ((+) strength) (buildBridges (removeSelectedPiece components piece) newVal)
  where
    newVal = if ((fst piece)==ports) then (snd piece) else (fst piece)
    strength = (fst piece)+(snd piece)
      
buildBridges2 :: [(Int,Int)] -> Int -> [(Int,Int)]
buildBridges2 components ports
  | validComponents == [] = [(0,0)]
  | otherwise = foldr1 (++) (map (doSomething2 components ports) validComponents)
  where
    validComponents = extractValidPieces components ports

doSomething2 :: [(Int, Int)] -> Int -> (Int,Int) -> [(Int,Int)]
doSomething2 components ports piece = map (\(str, len) -> ((str + strength), (len + 1))) (buildBridges2 (removeSelectedPiece components piece) newVal)
  where
    newVal = if ((fst piece)==ports) then (snd piece) else (fst piece)
    strength = (fst piece)+(snd piece)
      
part1 :: [(Int, Int)] -> Int
part1 components = foldr1 max (buildBridges components 0)

part2 :: [(Int,Int)] -> Int
part2 components = fst best
  where
    bridges = buildBridges2 components 0
    best = strongest (longest bridges [(0,0)]) (0,0)

longest :: [(Int, Int)] -> [(Int,Int)] -> [(Int,Int)]
longest [] long = long
longest ((str,len):xs) ((s,l):longs)
  | len==l = longest xs ((str,len):((s,l):longs))
  | len>l = longest xs [(str,len)]
  | otherwise = longest xs ((s,l):longs)

strongest :: [(Int, Int)] -> (Int,Int) -> (Int,Int)
strongest [] strong = strong
strongest ((str,len):xs) (s,l)
  | str>s = strongest xs (str,len)
  | otherwise = strongest xs (s,l)
