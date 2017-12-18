module Day18Input where

data Instruction = Snd Int | Sndr Char | Set (Char, Int) |  Setr (Char, Char) | Add (Char, Int) | Addr (Char, Char) |  Mul (Char, Int) | Mulr (Char, Char) | Mod (Char, Int) | Modr (Char, Char) | Rcv Char | Jgz (Char, Int) | Jgzr (Char, Char) | Jgzi (Int, Int) | Jgzir (Int, Char)

testInput :: [Instruction]
testInput = [(Set ('a', 1)), (Add ('a', 2)), (Mulr ('a', 'a')), (Mod ('a', 5)), (Sndr 'a'), (Set ('a', 0)), (Rcv 'a'), (Jgz ('a', (-1))), (Set ('a', 1)), (Jgz ('a', (-2)))]

input :: [Instruction]
input = [(Set ('i', 31)),
         (Set ('a', 1)),
         (Mul ('p', 17)),
         (Jgzr ('p', 'p')),
         (Mul ('a', 2)),
         (Add ('i', (-1))),
         (Jgz ('i', (-2))),
         (Add ('a', (-1))),
         (Set ('i', 127)),
         (Set ('p', 622)),
         (Mul ('p', 8505)),
         (Modr ('p', 'a')),
         (Mul ('p', 129749)),
         (Add ('p', 12345)),
         (Modr ('p','a')),
         (Setr ('b','p')),
         (Mod ('b', 10000)),
         (Sndr 'b'),
         (Add ('i', (-1))),
         (Jgz ('i', (-9))),
         (Jgz ('a', 3)),
         (Rcv 'b'),
         (Jgz ('b', (-1))),
         (Set ('f', 0)),
         (Set ('i', 126)),
         (Rcv 'a'),
         (Rcv 'b'),
         (Setr ('p', 'a')),
         (Mul ('p', (-1))),
         (Addr ('p','b')),
         (Jgz ('p', 4)),
         (Sndr 'a'),
         (Setr ('a','b')),
         (Jgzi (1, 3)),
         (Sndr 'b'),
         (Set ('f', 1)),
         (Add ('i', (-1))),
         (Jgz ('i', (-11))),
         (Sndr 'a'),
         (Jgz ('f', (-16))),
         (Jgz ('a', (-19)))]
