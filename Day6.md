# Day 6 - Memory Reallocation
## Part 1
In Part 1 we're trying to find where a process loops. The process has us finding the highest number in a list and redistributing it across all the "banks". We know that this process gets into an infinite loop - the puzzle asks us how many loops before we see a repeated distribution.

### Thoughts
So, we need to `findHighest` from a `[Int]`. We will need the value and the position `(Int, Int)` and it helps to give seed values - `findHighest :: [Int] -> Int -> (Int, Int) -> (Int, Int)` => `(findHighest banks position (seedvalue, seedposition))`.

Once we have the Highest, we can look to redistribute. I thought about trying to do it by rebuilding a list from slices with the appropriate resdistributed amounts but transferring the idea from head to code was... tricky. So I resorted to a more mechanical method of literally redistributing which is `O(value)` which... which gives us `balance :: [Int] -> [Int]` and `iBalance :: Int -> -> Int -> [Int] -> [Int]` `(iBalance valueToRedistribute currentPosition banks). `balance` provides the seed values to use in iBalance - using `findBalance` and then building a new list with a zero at the position which was the highest value previously.

Once redistributed we need to compare the new distribution with the list of previous distributions and then store it and repeat or admit there's a loop. This gives us `findRepeat :: Int -> [[Int]] -> [Int] -> (Int, [[Int]])` or `(findRepeat count memory banks)`. This function originally just returned the count, but...

## Part 2
Part 2 asks us what is the length of the loop. I altered `findRepeat` to this version which also returns the memory therefore allowing us to count the length of the loop with `getLoopLength :: Int -> [Int] -> [[Int]] -> Int` or `(getLoopLength count startOfLoop memory)`.
