import Scaffold

newSpiral :: Int -> [[[Int]]]
newSpiral x = [[[x],[x],[x],[x]]]

insertSpiral :: Int -> [[[Int]]] -> [[[Int]]]
insertSpiral x spiral 
  | edge == 0 = (init spiral) ++ [[(((last spiral)!!0) ++ [x]), ((last spiral)!!1), ((last spiral)!!2), ((last spiral)!!3)]] 
  | edge == 1 = (init spiral) ++ [[((last spiral)!!0), (((last spiral)!!1) ++ [x]), ((last spiral)!!2), ((last spiral)!!3)]]
  | edge == 2 = (init spiral) ++ [[((last spiral)!!0), ((last spiral)!!1), (((last spiral)!!2) ++ [x]), ((last spiral)!!3)]]
  | edge == 3 = (init spiral) ++ [[((last spiral)!!0), ((last spiral)!!1), ((last spiral)!!2), (((last spiral)!!3) ++ [x])]]
  | edge == 4 = spiral ++ [[[x],[],[],[]]]
  where edge = findCurrentEdge (last spiral) (2 * ((length spiral) - 1))

findCurrentEdge :: [[Int]] -> Int -> Int
findCurrentEdge xs len
  | (length (xs!!0)) < len = 0
  | (length (xs!!1)) < len = 1
  | (length (xs!!2)) < len = 2
  | (length (xs!!3)) < len = 3
  | otherwise = 4

findCurrentLayer :: [[[Int]]] -> Int
findCurrentLayer spiral = (length spiral) - 1

listToSpiral :: [Int] -> [[[Int]]] -> [[[Int]]]
listToSpiral [] spiral = spiral
listToSpiral (x:xs) spiral = listToSpiral xs (insertSpiral x spiral)

corner :: Int -> [[Int]] -> Int
corner x layer = last (layer!!x)
-- if the corner doesn't exist yet, this will give the wrong answer or an error

getAnEdge :: Int -> Int -> [[[Int]]] -> [Int]
getAnEdge edge layer spiral = (spiral!!layer)!!edge

getLayer :: Int -> [[[Int]]] -> [[Int]]
getLayer layer spiral = spiral!!layer

-- getNextNumber :: [[[Int]]] -> Int
-- getNextNumber spiral = 

getNextEmpty :: [[[Int]]] -> (Int, Int, Int)
getNextEmpty spiral
  | edge == 4 = ((layer + 1), 0, 0)
  | otherwise = (layer, edge, position)
  where
    edge = findCurrentEdge (last spiral) (2 * ((length spiral) - 1))
    layer = findCurrentLayer spiral
    position = length (getAnEdge edge layer spiral)

getTouching :: [[[Int]]] -> [Int]
getTouching spiral
  | (edge == 0) && (position == 0) = [last ((spiral!!(layer - 1))!!3), (((spiral!!(layer - 1))!!0)!!0)]
  | (position == 0) = [(last ((spiral!!layer)!!(edge - 1))), (last (init ((spiral!!layer)!!(edge - 1)))), (last ((spiral!!(layer - 1)!!(edge - 1)))), ((spiral!!(layer - 1))!!edge)!!0]
  | (edge == 0) && (position == 1) = [(((spiral!!layer)!!(edge))!!0), (last ((spiral!!(layer - 1)!!3))), (((spiral!!(layer - 1))!!edge)!!0), (((spiral!!(layer - 1))!!edge)!!1)]
  | (position == 1) = [(((spiral!!layer)!!(edge))!!0), (last ((spiral!!(layer - 1)!!(edge - 1)))), (((spiral!!(layer - 1))!!edge)!!0), (((spiral!!(layer - 1))!!edge)!!1)]
  | edge == 0 = [(((spiral!!layer)!!(edge))!!(position - 1))] ++ (take 3 (drop (position - 2) ((spiral!!(layer - 1))!!edge)))
  | (edge == 3) && ((length (((spiral!!layer)!!0))) - 1) == position = [(((spiral!!layer)!!(edge))!!(position - 1)), (((spiral!!layer)!!0)!!0)] ++ (take 3 (drop (position - 2) ((spiral!!(layer - 1))!!edge)))
  | (edge == 3) && ((length (((spiral!!layer)!!0))) - 2) == position = [(((spiral!!layer)!!(edge))!!(position - 1)), (((spiral!!layer)!!0)!!0), (((spiral!!(layer-1))!!edge)!!(position - 1)) , (((spiral!!(layer-1))!!edge)!!(position - 2))]
  | otherwise = [(((spiral!!layer)!!(edge))!!(position - 1))] ++ (take 3 (drop (position - 2) ((spiral!!(layer - 1))!!edge)))
  where (layer, edge, position) = getNextEmpty spiral

findFirstNumber :: Int -> [[[Int]]] -> (Int, [[[Int]]])
findFirstNumber num spiral
  | n > num = (n, (insertSpiral n spiral))
  | otherwise = findFirstNumber num (insertSpiral n spiral)
  where
    n = foldr1 (+) (getTouching spiral)

fullLayer :: [Int] -> Bool
fullLayer l = iFullLayer 0 1 (length l)

nearlyFullLayer :: [Int] -> Bool
nearlyFullLayer l = iFullLayer 0 1 ((length l) + 1)

iFullLayer :: Int -> Int -> Int -> Bool
iFullLayer n 0 l = True
iFullLayer n m l 
	   | l == m	= True
	   | l > m 	= iFullLayer (n + 1) (((2 * n) + 1) ^ 2) l
	   | otherwise	= False

