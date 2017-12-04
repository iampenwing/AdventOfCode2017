# Day 4 - High Entropy Passphrases
## Puzzle 1
Simple one this. Count the number of valid passphrases. A passphrase is a list of words (strings). A valid passphrase is a passphrase with no repeated words.

Simple recursive membership testing. Can probably be done in a one-liner.

Biggest stumbling block - converfting the input file to the `[[[Char]]]` format. Somehow in the conversion, I lost the last letter of each word and so was getting the wrong answer.

The answer can be reached by a straightforward application of `countFalse (map areRepeats input)`

## Puzzle 2
This adds another requirement - none of the words can be anagrams of each other. Straightforward again - just sort each word and run it. Words are quite short so an insertion sort is fine.

The answser then becomes `countFalse (map areRepeats (map (\x -> map sortString x) input))`

