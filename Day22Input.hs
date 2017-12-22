module Day22Input where

testInput :: [[Bool]]
testInput = [[False, False, True], [True, False, False], [False, False, False]]

testInput2 :: [[Int]]
testInput2 = [[0,0,3],[3,0,0],[0,0,0]]

input :: [[Bool]]
input = [[False,False,False,False,True,False,True,False,False,False,True,True,False,False,True,True,True,True,True,True,False,True,False,False,False],[False,True,True,True,True,True,False,True,False,True,False,False,True,False,False,True,False,True,True,False,True,False,True,True,False],[False,True,False,False,False,True,True,True,True,True,False,True,True,False,False,False,False,True,False,True,True,True,False,True,True],[False,True,True,False,True,True,False,True,False,True,True,True,True,False,True,True,True,True,False,True,True,False,True,True,True],[True,False,True,False,True,True,False,True,False,False,False,False,True,True,True,False,True,False,False,True,True,True,True,False,True],[False,False,True,True,True,False,False,False,True,True,False,False,True,True,False,False,False,True,False,True,True,False,True,True,False],[False,False,False,False,False,False,False,True,True,False,True,True,True,False,True,True,True,False,True,True,True,False,True,True,True],[True,False,True,False,True,False,True,False,False,False,False,False,True,False,True,False,False,True,True,True,True,False,True,False,False],[True,True,False,False,False,True,False,False,True,False,True,False,False,True,True,False,False,True,False,True,False,True,True,True,True],[True,False,False,True,False,False,False,False,False,True,True,False,True,True,True,False,False,False,False,False,True,False,True,True,True],[False,True,False,True,True,False,False,True,True,False,True,True,True,True,True,False,False,True,True,False,True,True,True,False,False],[True,True,False,False,False,False,True,True,True,False,False,False,False,True,False,True,False,True,False,True,True,True,True,False,True],[True,True,True,False,True,False,True,False,False,True,False,True,False,True,False,True,True,True,False,False,False,False,True,True,False],[True,True,False,True,True,False,False,False,True,False,True,True,False,True,False,True,True,False,False,False,True,False,False,False,False],[True,True,False,False,False,False,True,False,True,False,True,False,True,True,True,False,False,False,True,True,True,False,True,True,True],[False,False,True,True,False,True,True,True,False,True,False,True,True,True,True,False,False,False,True,True,True,False,False,True,False],[False,False,True,True,False,False,False,True,True,False,False,False,True,False,False,True,True,False,True,False,True,True,True,True,True],[False,True,True,False,False,False,True,True,False,False,True,False,True,False,True,False,False,True,True,False,False,False,True,False,False],[True,False,True,False,True,False,True,False,False,True,True,True,True,True,False,False,True,True,True,True,False,False,True,True,True],[False,False,True,True,True,False,True,False,True,False,False,False,False,True,False,True,True,True,True,False,False,True,False,False,True],[False,False,False,False,True,False,True,False,True,True,True,False,False,False,True,False,False,True,False,False,True,False,True,False,True],[True,True,True,False,True,True,False,False,True,True,True,False,False,True,True,False,False,False,True,True,False,False,True,False,True],[False,False,False,True,False,True,True,False,False,True,False,True,True,False,True,True,True,False,False,False,False,False,False,True,False],[False,True,False,False,True,True,False,True,True,True,True,True,False,True,False,True,True,False,False,False,False,False,True,False,True],[True,False,False,False,False,False,True,True,False,False,True,True,True,True,False,True,True,False,True,True,True,False,False,True,False]]

input2 :: [[Int]]
input2 = [[0,0,0,0,3,0,3,0,0,0,3,3,0,0,3,3,3,3,3,3,0,3,0,0,0],[0,3,3,3,3,3,0,3,0,3,0,0,3,0,0,3,0,3,3,0,3,0,3,3,0],[0,3,0,0,0,3,3,3,3,3,0,3,3,0,0,0,0,3,0,3,3,3,0,3,3],[0,3,3,0,3,3,0,3,0,3,3,3,3,0,3,3,3,3,0,3,3,0,3,3,3],[3,0,3,0,3,3,0,3,0,0,0,0,3,3,3,0,3,0,0,3,3,3,3,0,3],[0,0,3,3,3,0,0,0,3,3,0,0,3,3,0,0,0,3,0,3,3,0,3,3,0],[0,0,0,0,0,0,0,3,3,0,3,3,3,0,3,3,3,0,3,3,3,0,3,3,3],[3,0,3,0,3,0,3,0,0,0,0,0,3,0,3,0,0,3,3,3,3,0,3,0,0],[3,3,0,0,0,3,0,0,3,0,3,0,0,3,3,0,0,3,0,3,0,3,3,3,3],[3,0,0,3,0,0,0,0,0,3,3,0,3,3,3,0,0,0,0,0,3,0,3,3,3],[0,3,0,3,3,0,0,3,3,0,3,3,3,3,3,0,0,3,3,0,3,3,3,0,0],[3,3,0,0,0,0,3,3,3,0,0,0,0,3,0,3,0,3,0,3,3,3,3,0,3],[3,3,3,0,3,0,3,0,0,3,0,3,0,3,0,3,3,3,0,0,0,0,3,3,0],[3,3,0,3,3,0,0,0,3,0,3,3,0,3,0,3,3,0,0,0,3,0,0,0,0],[3,3,0,0,0,0,3,0,3,0,3,0,3,3,3,0,0,0,3,3,3,0,3,3,3],[0,0,3,3,0,3,3,3,0,3,0,3,3,3,3,0,0,0,3,3,3,0,0,3,0],[0,0,3,3,0,0,0,3,3,0,0,0,3,0,0,3,3,0,3,0,3,3,3,3,3],[0,3,3,0,0,0,3,3,0,0,3,0,3,0,3,0,0,3,3,0,0,0,3,0,0],[3,0,3,0,3,0,3,0,0,3,3,3,3,3,0,0,3,3,3,3,0,0,3,3,3],[0,0,3,3,3,0,3,0,3,0,0,0,0,3,0,3,3,3,3,0,0,3,0,0,3],[0,0,0,0,3,0,3,0,3,3,3,0,0,0,3,0,0,3,0,0,3,0,3,0,3],[3,3,3,0,3,3,0,0,3,3,3,0,0,3,3,0,0,0,3,3,0,0,3,0,3],[0,0,0,3,0,3,3,0,0,3,0,3,3,0,3,3,3,0,0,0,0,0,0,3,0],[0,3,0,0,3,3,0,3,3,3,3,3,0,3,0,3,3,0,0,0,0,0,3,0,3],[3,0,0,0,0,0,3,3,0,0,3,3,3,3,0,3,3,0,3,3,3,0,0,3,0]]