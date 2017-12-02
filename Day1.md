# Day 1 - Inverse Captcha
## Puzzle Part 1
So, the puzzle in Part 1 is to find the sum of all digits in a sequence where digit n == digit n+1 and where the sequence wraps round.

### Thoughts
So, I need to "rotate" a list of numbers, then compare the two lists and only add numbers which match. So I need:
1. To rotate a list of numbers
2. To zip the two lists together
3. To find all elements that are (X,X)
4. To take add all those Xs

I had a lot of helper functions to help with each of these steps early in the process, what's left is the squished down version. The only thing that stands as its own function really is the `rotateList` function. Initially, this only rotated it by 1 so it was only `[Int]->[Int]`. It changed for Part 2 below.

Once I had this and it was working with my list of numbers I got the input. As a literal String. A string is just a `[Char]`, but I need `[Int]`. I finally find a way to do it and then separate it out to Scaffolding and quickly learn about Haskell Modules.

Anyway, It's done!

## Puzzle Part 2
Part 2 asks the same, but instead of the list rotating by 1, it's rotating by half the length.

### Thoughts
I have to fix up `rotateList` to become `Int->[Int]->[Int]` and supply half the list length. Initially my solution didn't work - It actually reversed the new last half because I got my appending in the wrong place, but once that was solved it was fairly straight forward.

Helps that we have guaranteed even length input to work with...

# Later Adjustments
Looking at another person's solution (@nonphatic@cybre.space) I discovered `drop :: Int -> [a] -> [a]` and `take Int -> [a] -> [a]` as a much better way of rotating the list than the recursive solution I originally created.

So I stole it...