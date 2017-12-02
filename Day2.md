# Day 2 - Checksum
## Puzzle Part 1
Here we're looking at generating a "checksum" for a "spreadsheet". The Checksum is the sum of the differences between the highest and lowest values of each row.

### Thoughts
Well... we're working with `[[Int]]` here right... Lots of `map`s to come...

First up though, I need to establish the Min and Max of a list... this sounds like `[Int] -> (Int, Int)` mapping that over `[[Int]]` should then give us a `[(Int, Int)] which I can then throw the subtract function over to get `[Int]` and then `foldr1` it with `(+)` again...

An "internal" function `iListMinMax` and a "caller" function which seeds the minimum and maximum values with the first list item.

And guards... yay for guards!

On the input I just manipulated the displayed input from the site and added the list markers and separators unsing emacs' find/replace function. Couldn't do this with the string of digits... well, I might have been able to if I'd looked harder...

##Puzzle Part 2
OK, now it's wanting the **only** "evenly divisable values" - i.e. the only values which divide into an integer - or Remainder zero.

Very little survived from the initial problem. other than the structure of the data I was throwing around. I needed a completely different way to fill the `(Int,Int)` structure. Again, guards to the rescue and recursion!

The problems specified **only** pair that were evenly divisable which was reassuring as it promised exactly 1 pair in each row.