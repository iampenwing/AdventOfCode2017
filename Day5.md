# Day 5 - A Maze of Twisty Trampolines, All Alike
## Part 1

Traversing a list until you "exit" it following the rule of jumping by the amount at your current position and incrementing that value by 1. It's a little bit "Langton's Ant" and is horrifically iterative for Haskell but remarkably simple:

```
step :: (Int, [Int]) -> (Int, [Int])
step (n, []) = (-1, [])
step (n, (x:xs)) = ((n + (x:xs)!!n), ((take (n) (x:xs)) ++ [(((x:xs)!!n)+1)] ++ (drop n xs)))

escape :: (Int, [Int]) -> Int -> Int
escape (x, l) n
  | (x < 0) || (x >= length l) = n
  | otherwise = escape (step (x, l)) (n + 1)
```

## Part 2
The rule changes from adding 1 to the current value to adding one unless the value is 3 or more when you should minus 1 instead... Oh simplicity itself to do...

```
part2step :: (Int, [Int]) -> (Int, [Int])
part2step (n, []) = (-1, [])
part2step (n, (x:xs)) 
  | (x:xs)!!n >= 3 = ((n + (x:xs)!!n), ((take (n) (x:xs)) ++ [(((x:xs)!!n)-1)] ++ (drop n xs)))
  | otherwise = ((n + (x:xs)!!n), ((take (n) (x:xs)) ++ [(((x:xs)!!n)+1)] ++ (drop n xs)))

```

Except that, when it eventually spat out an answer rather than the lovely `Int` I was expecting, I got: `** Exception: stack overload`

But there's tail recursion and Haskell's optimisation means that the stack should surely not be too much of an issue??

I tried out a limit on the number of steps (`part2escape2` - to then run with the new, partially solved state afresh and add the max steps), but it was another long wait and I coded up a quick perl script (eventually) to try and get round the stack problem that way... Still waiting as I type this up...

Gave up in the end and resorted to a perl script... with far too many errors because it's been too long. In the end biggest problem was, I think, trying to use `++`;

