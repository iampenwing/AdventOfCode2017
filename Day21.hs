import Day21Input

getGrids :: [[Bool]] -> [[[[Bool]]]]
getGrids pic
  | (mod (length pic) 2) == 0 = getGrids2 pic 
  | (mod (length pic) 3) == 0 = getGrids3 pic 

getGrids2 :: [[Bool]] -> [[[[Bool]]]]
getGrids2 [] = []
getGrids2 (row1:(row2:pics)) = (splitRow2 row1 row2):(getGrids2 pics)

getGrids3 :: [[Bool]] -> [[[[Bool]]]]
getGrids3 [] = []
getGrids3 (row1:(row2:(row3:pics))) = (splitRow3 row1 row2 row3):(getGrids3 pics)

splitRow2 :: [Bool] -> [Bool] -> [[[Bool]]]
splitRow2 [] [] = []
splitRow2 row1 row2 = [[(row1!!0),(row1!!1)],[(row2!!0),(row2!!1)]]:(splitRow2 (drop 2 row1) (drop 2 row2))

splitRow3 :: [Bool] -> [Bool] -> [Bool] -> [[[Bool]]]
splitRow3 [] [] [] = []
splitRow3 row1 row2 row3 = [[(row1!!0),(row1!!1),(row1!!2)],[(row2!!0),(row2!!1),(row2!!2)],[(row3!!0),(row3!!1),(row3!!2)]]:(splitRow3 (drop 3 row1) (drop 3 row2) (drop 3 row3))

buildPic :: [[[[Bool]]]] -> [[Bool]]
buildPic [] = []
buildPic (row:rows) = (buildRow row) ++ (buildPic rows)

buildRow :: [[[Bool]]] -> [[Bool]]
buildRow ([]:_) = []
buildRow row = (foldr1 (++) (map head row)) : buildRow (map tail row)


--buildPic2 :: [[[Bool]]] -> Int -> [[Bool]]
--buildPic2 [] _ = []
--buildPic2 grid n = (mergeGrids (take n grid)) ++ (buildPic2 (drop n grid))

--mergeGrids :: [[[Bool]]] -> [[Bool]]
--mergeGrids [] = x
--mergeGrids grid
--  | (length (grid!!0)) == 3 = [[((grid!!0)!!0)++((grid!!1)!!0)++((grid!!2)!!0)],
--                            [((grid!!0)!!1)++((grid!!1)!!1)++((grid!!2)!!1)],
--                            [((grid!!0)!!2)++((grid!!1)!!2)++((grid!!2)!!2)]]
--  | (length (grid!!0)) == 4 = [[((grid!!0)!!0)++((grid!!1)!!0)++((grid!!2)!!0),((grid!!3)!!0)],
--                            [((grid!!0)!!1)++((grid!!1)!!1)++((grid!!2)!!1),((grid!!3)!!1)],
--                            [((grid!!0)!!2)++((grid!!1)!!2)++((grid!!2)!!2),((grid!!3)!!2)],
--                            [((grid!!0)!!3)++((grid!!1)!!3)++((grid!!2)!!3),((grid!!3)!!3)]]


rotate :: [[Bool]] -> [[Bool]]
rotate grid
  | (mod (length grid) 2) == 0 = [[((grid!!1)!!0),((grid!!0)!!0)],[((grid!!1)!!1),((grid!!0)!!1)]]
  | (mod (length grid) 3) == 0 = [[((grid!!2)!!0),((grid!!1)!!0),((grid!!0)!!0)],
                                  [((grid!!2)!!1),((grid!!1)!!1),((grid!!0)!!1)],
                                  [((grid!!2)!!2),((grid!!1)!!2),((grid!!0)!!2)]]

flipGridX :: [[Bool]] -> [[Bool]]
flipGridX grid
  | (mod (length grid) 2) == 0 = [[((grid!!1)!!0),((grid!!1)!!1)],[((grid!!0)!!0),((grid!!0)!!1)]]
  | (mod (length grid) 3) == 0 = [[((grid!!2)!!0),((grid!!2)!!1),((grid!!2)!!2)],
                                  [((grid!!1)!!0),((grid!!1)!!1),((grid!!1)!!2)],
                                  [((grid!!0)!!0),((grid!!0)!!1),((grid!!0)!!2)]]

flipGridY :: [[Bool]] -> [[Bool]]
flipGridY grid
  | (mod (length grid) 2) == 0 = [[((grid!!0)!!1),((grid!!0)!!0)],[((grid!!1)!!1),((grid!!1)!!0)]]
  | (mod (length grid) 3) == 0 = [[((grid!!0)!!2),((grid!!0)!!1),((grid!!0)!!0)],
                                  [((grid!!1)!!2),((grid!!1)!!1),((grid!!1)!!0)],
                                  [((grid!!2)!!2),((grid!!2)!!1),((grid!!2)!!0)]]

--enhanceGrid :: [[Bool]] -> [([[Bool]],[[Bool]])] -> [[Bool]]
--enhanceGrid grid instructions
--  | (mod (length grid) 2) == 0 = enhanceGrid2 grid instructions
--  | (mod (length grid) 3) == 0 = enhanceGrid3 grid instructions
  
enhanceGrid :: [[Bool]] -> [([[Bool]],[[Bool]])] -> [[Bool]]
enhanceGrid grid [] = error "Out of Instructions"
enhanceGrid grid ((thePattern, enhancement):instructions)
  | (length thePattern) /= (length grid) = enhanceGrid grid instructions
  | grid==thePattern = enhancement
  | (rotate grid)==thePattern = enhancement
  | (flipGridX grid)==thePattern = enhancement
  | (flipGridY grid)==thePattern = enhancement
  | (rotate (rotate grid))==thePattern = enhancement
  | (rotate (flipGridX grid))==thePattern = enhancement
  | (rotate (flipGridY grid))==thePattern = enhancement
  | (rotate (rotate (rotate grid)))==thePattern = enhancement
  | otherwise = enhanceGrid grid instructions
  
--enhanceGrid3 :: [[Bool]] -> [([[Bool]],[[Bool]])] -> [[Bool]]
--enhanceGrid3 grid ((thePattern, enhancement):instructions)
--  | (length thePattern) /= (length grid) = enhanceGrid3 grid instructions
--  | grid==thePattern = enhancement
--  | (rotate grid)==thePattern = enhancement
--  | (rotate (rotate grid))==thePattern = enhancement
--  | (rotate (rotate (rotate grid)))==thePattern = enhancement
--  | (flipGridX grid)==thePattern = enhancement
--  | (flipGridX (rotate grid))==thePattern = enhancement
--  | (flipGridX (rotate (rotate grid)))==thePattern = enhancement
--  | (flipGridX (rotate (rotate (rotate grid))))==thePattern = enhancement
--  | (flipGridY grid)==thePattern = enhancement
--  | (flipGridY (rotate grid))==thePattern = enhancement
--  | (flipGridY (rotate (rotate grid)))==thePattern = enhancement
--  | (flipGridY (rotate (rotate (rotate grid))))==thePattern = enhancement
--  | otherwise = enhanceGrid3 grid instructions

enhancePic :: [[Bool]] -> [([[Bool]],[[Bool]])] -> [[Bool]]
enhancePic pic instructions = buildPic (enhanceGrids (getGrids pic) instructions)

enhanceGrids :: [[[[Bool]]]] -> [([[Bool]],[[Bool]])] -> [[[[Bool]]]]
enhanceGrids [] _ = []
enhanceGrids (row:rows) instructions = (enhanceRow row instructions):(enhanceGrids rows instructions)

enhanceRow :: [[[Bool]]] -> [([[Bool]],[[Bool]])] -> [[[Bool]]]
enhanceRow [] _ = []
enhanceRow (grid:grids) instructions = (enhanceGrid grid instructions):(enhanceRow grids instructions)

multiEnhance :: [[Bool]] -> [([[Bool]],[[Bool]])] -> Int -> [[Bool]]
multiEnhance pic _ 0 = pic
multiEnhance pic instructions count = multiEnhance (enhancePic pic instructions) instructions (count - 1)

countOnBits :: [[Bool]] -> Int
countOnBits [] = 0
countOnBits (row:rows) = (foldr1 (+) (map (\x -> if x then 1 else 0) row)) + countOnBits rows
