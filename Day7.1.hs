
import Day7Input

data Day7Node = Branch [Char] Int [Day7Node]
data Day7Weights = WeightNode Int | WeightBranch (Int, [Int])

part1Nodes :: [([Char], Int, [[Char]])] -> ([[Char]], [[Char]])
part1Nodes [] = ([[]], [[]])
part1Nodes ((thisNode, _, branches):xs) =
  ((thisNode:theseNodes), (branches ++ otherBranches))
  where
    (theseNodes, otherBranches) = part1Nodes xs

findBase :: ([[Char]], [[Char]]) -> [Char]
findBase ([], _) = []
findBase ((node:nodes), branches)
  | elem node branches = findBase (nodes, branches)
  | otherwise = node

trim :: [[Char]] -> [[Char]]
trim [] = []
trim ("":xs) = trim xs
trim (x:xs) = x:(trim xs)

getNodeDetails :: [Char] -> [([Char], Int, [[Char]])] -> ([Char], Int, [[Char]])
getNodeDetails _ [] = ([], 0, [])
getNodeDetails id thenodes
  | id == nid = (nid, nweight, nbranches)
  | otherwise = getNodeDetails id (tail thenodes)
  where
    (nid, nweight, nbranches) = head thenodes
    
initTree :: [([Char], Int, [[Char]])] -> [Char] -> Day7Node
initTree allnodes id
  | nodebranches == [] = Branch nodeid nodeweight []
  | otherwise = Branch nodeid nodeweight (map (initTree allnodes) nodebranches)
  where
    (nodeid, nodeweight,nodebranches) = getNodeDetails id allnodes

weightTree :: Day7Node -> Day7Weights
weightTree (Branch _ weight []) = WeightNode weight
weightTree (Branch _ weight branches) = WeightBranch ((weight + (foldr1 (+) (map getWeight branchWeights))), (map getWeight branchWeights))
  where
    branchWeights = map weightTree branches

getWeight :: Day7Weights -> Int
getWeight (WeightNode weight) = weight
getWeight (WeightBranch (weight, _)) = weight

-- extractWeights :: Day7Weights -> [Int]

puzzle1 :: [([Char], Int, [[Char]])] -> [Char]
puzzle1 x = findBase (nodes, (trim branches))
  where
    (nodes, branches) = part1Nodes x

