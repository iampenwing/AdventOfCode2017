# Day 3 - Spiral Memory
## Puzzle 1
This is allegedly and "experimental kind of memory" designed by idiots. The only place to access is at memory position 1 - the centre of a spiral. "Space efficient" apparantly (because that matters on an infinite 2D grid"), as all spaces are filled (so no thought of fragmentation)...

Anyhoots, I need to work out the manhatten distance from arbitary position to the access point at position 1.

### Thoughts
Each "layer" of the spiral is a list:

```
spiralLayer :: Int -> [Int]
spiralLayer 0 = [1..1]
spiralLayer n = [((((2 * n) - 1) ^ 2) + 1) .. (((2 * n) + 1) ^ 2)]
```

Each "edge" has a manhatten distence pattern based on `n`:

```
edgePattern :: Int -> [Int]
edgePattern 0 = [0]
edgePattern 1 = [2, 1, 2]
edgePattern n = [(2 * n), ((2 * n) - 1) .. n] ++ [(n + 1) .. (2 * n)]
```

But the whole  layer is made up of four edges where the corners only appear once and it starts just inside a corner. So each layer is:

```
layerPattern :: Int -> [Int]
layerPattern 0 = [0]
layerPattern n = 
	     let e = edgePattern (n) in
	     drop 1 e ++ drop 1 e ++ drop 1 e ++ drop 1 e
```

So, now, I should be able to find the layer containing our arbitary location, I can zip that layer with the edgePattern and pull out the right distence:

```
findLayer :: Int -> Int
findLayer 1 = 0
findLayer x = iFindLayer 1 x

iFindLayer :: Int -> Int -> Int
iFindLayer n x
	   | (((2 * n) + 1) ^ 2) > x	= n
	   | otherwise 	   	= iFindLayer (n + 1) x

extractValue :: Int -> [(Int, Int)] -> Int
extractValue k [] = 0
extractValue k (pair:pairs) 
	     | k == (fst pair)  = snd pair
	     | otherwise        = extractValue k pairs

getLayerPattern :: Int -> [(Int,Int)]
getLayerPattern n = zip (spiralLayer n) (layerPattern n)

puzzle1 :: Int -> Int
puzzle1 n = extractValue n (getLayerPattern (findLayer n))
```

I set `extractValue` up in Scaffold.hs as it may come in handy later...

Some debugging was needed to tweak some erros but this is the final code 8-)

## Part 2 
So, starting at position 1 we're going to fill the spiral. The centre takes the value 1 and the next location take the value of the sum of all its neighbours' current values...

I need to find the first written value higher than a particular input value.

### Thoughts
Eugh... this is not a nice one... 

So, I need to generate the next spiral given an existing spiral list

```
nextSpiralValue :: [Int] -> Int
nextSpiralValue [] = 1
nextSpiralValue [1] = [1, 2]
-- Something...
```

It will help to have the corners for each layer identified:

```
getCorners :: Int -> [Int]
getCorners 0 = []
getCorners 1 = [2, 3, 5, 7]
getCorners n = let l = (findLayer n) in
	   [l!!((2 * n) - 1), l!!((4 * n) - 1), l!!((6 * n) - 1)]
```

The start of each layer has the head and last positions from the previous layer touching it:
```
nextSpiralValue spiral
		| fullLayer spiral	= (head (spiralLayer (findLayer (length spiral)))) + (last (spiralLayer (findLayer (length spiral))))

fullLayer :: [Int] -> Bool
fullLayer l = iFullLayer 0 1 (length l)

iFullLayer :: Int -> Int -> Int -> Bool
iFullLayer n 0 = True
iFullLayer n m l 
	   | l == m	= True
	   | l > m 	= iFullLayer (n + 1) (((2 * n) + 1) ^ 2) l
	   | otherwise	= False
```

The last of each layer has the end of the previous layer, the start of the current layer and the last position:

```
nextSpiralValue spiral
		| lastOnLayer spiral	= (spiral!!(first (spiralLayer (findLayer (length spiral))))) + (spiral!!(last (spiralLayer (findLayer ((length spiral) -1))) + (last spiral)

lastOnLayer :: [Int] -> Bool
lastOnLayer l = iLastOnLayer 0 1 (length l)

iLastOnLayer :: Int -> Int -> Int -> Bool
iLastOnLayer n 0 = True
iLastOnLayer n m l 
	   | l == (m - 1)	= True
	   | l > m 		= iLastOnLayer (n + 1) (((2 * n) + 1) ^ 2) l
	   | otherwise		= False
```

Each of the three corners has the same corner from the previous layer and the previous position in the spiral:

```
nextSpiralValue spiral
		| getPrevCorner spiral != 0	= spiral!!(getCorner spiral) + (last spiral)
	
-- getPrevCorner will return 0 if it is not a corner, and the position of the previous layer's equivelant corner if it is a corner
getPrevCorner :: [Int] -> Int
getPrevCorner s = iGetPrevCorner ((findLayer s) - 1) (length s) s

iGetPrevCorner :: Int -> Int -> [Int] -> Int
iGetPrevCorner 0 n l = 0
iGetPrevCorner 1 n l
	       | n==2 = 1
	       | n==4 = 1  
	       | n==6 = 1
	       | otherwise = 0
iGetPrevCorner m n l 
	       | n 
	  

```

At this point I gave up and decided to rethink...

I decided to try setting up a structure where the spiral is a `[[[Int]]]` - basically, the spiral is a list of Layers (`[[Int]]`), which in turn are lists of edges (`[Int]`). Each edge starts just after the "turn" and ends on the corner piece. 

Code is in Day3.3.hs and currently isa giving the wrong answer. But it's late. Oh so late...

** Update: Early Morning, Day 5 ** - Code now works - established there was an error in generating the spiral on the last edge of a layer. I missed a test for the penultimate position of edge 3, and got part of the other cases for edge 3 incorrect... bleurgh

So, yeah... Spirals: Hard!



