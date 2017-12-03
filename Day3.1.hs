


import Scaffold

spiralLayer :: Int -> [Int]
spiralLayer 0 = [1..1]
spiralLayer n = [((((2 * n) - 1) ^ 2) + 1) .. (((2 * n) + 1) ^ 2)]

edgePattern :: Int -> [Int]
edgePattern 0 = [0]
edgePattern 1 = [2, 1, 2]
edgePattern n = [(2 * n), ((2 * n) - 1) .. n] ++ [(n + 1) .. (2 * n)]

layerPattern :: Int -> [Int]
layerPattern 0 = [0]
layerPattern n = 
	     let e = edgePattern (n) in
	     drop 1 e ++ drop 1 e ++ drop 1 e ++ drop 1 e

findLayer :: Int -> Int
findLayer 1 = 0
findLayer x = iFindLayer 1 x

iFindLayer :: Int -> Int -> Int
iFindLayer n x
	   | (((2 * n) + 1) ^ 2) > x	= n
	   | otherwise 	   	= iFindLayer (n + 1) x

getLayerPattern :: Int -> [(Int,Int)]
getLayerPattern n = zip (spiralLayer n) (layerPattern n)

puzzle1 :: Int -> Int
puzzle1 n = extractValue n (getLayerPattern (findLayer n))
