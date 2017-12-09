# Day 8 - I Heard You Like Registers
## Thoughts
I found this really straight forward. Again, the difficult bit was adjusting the input to a suitable format for Haskell. I started by trying to leave the comparators intact, but it just wasn't feasible so I resorted to numbers for the type of comparator and guards when running the instruction. Because I just wrote them this way, they worked as:

```
< : 1
> : 2
>= : 3
<= : 4
== : 5
/= (!=) : 6
```

I also made the rather embarassing mistake - originally I converted `dec XXX` to `((-) XXX)` forgetting that subtraction is not commutative. That's why you have the `((+) XXX)` construct for `inc XXX` but `(\x -> x - XXX)` construct for `dec XXX`. 

Part 1 wanted the highest register at the end of the instructions. Part 2 wanted the highest value ever encountered. As such the functions carry the relevant `highVal` and do the comparisons at each stage.

Relatively quick to programme this one - it was getting the input right that was the problem.
