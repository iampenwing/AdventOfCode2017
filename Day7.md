# Day 7 - Recursive Circus
## Puzzle 1
Eurgh... basically a tree structure where the branches should all weight the same. Each branch's weight is it's own node's weight plus the weight of all the sub-branches. All sub-branches should also weigh the same. The first task is to find the base of the tree.

I started by trying to do all sorts of complicated stuff. Then realised there was a cheating way to do it - build a list of nodes and a list of nodes in branches. The one node not in the branches list would be the root.

## Puzzle 2
Find the branch that is unbalanced and work out what it should be to fix it. I tried learning about `data` types and just gave up. I built the tree buit actually accessing it was a right nightmare. I found the unbalanced branch in the first two layers by careful extracting and examining in the interpreter... then hand calculated the rest. 

Today was definitely a cheats day. I really should consider working out `data` types properly...
