import Scaffold

input :: [Int]
input = [147,37,249,1,31,2,226,0,161,71,254,243,183,255,30,70]

input2 :: [Int]
input2 = (map fromEnum "147,37,249,1,31,2,226,0,161,71,254,243,183,255,30,70") ++ [17,31,73,47,23]


twist :: [Int] -> Int -> Int -> [Int]
twist [] _ _ = []
twist l pos len = rotateList ((length l) - pos) ((reverse (take len rl)) ++ (drop len rl))
  where
    rl = rotateList pos l

step :: [Int] -> [Int] -> Int -> Int -> [Int]
step h [] _ _ = h
step h (l:ls) p s = step (twist h p l) ls (mod (p + (l + s)) (length h)) (s + 1)

step2 :: [Int] -> [Int] -> Int -> Int -> ([Int], Int, Int)
step2 h [] p s = (h, p, s)
step2 h (l:ls) p s = step2 (twist h p l) ls (mod (p + (l + s)) (length h)) (s + 1)

puzzle1 :: Int
puzzle1 = ((x!!0) * (x!!1))
  where
    x = step [0..255] input 0 0

runMultiSteps :: [Int] -> [Int] -> Int -> Int -> Int -> ([Int], Int, Int)
runMultiSteps h l p s 0 = (h, p, s)
runMultiSteps h l p s count = runMultiSteps nh l np ns (count - 1)
  where
    (nh, np, ns) = step2 h l p s

getSparseHash :: [Int] -> [Int]
getSparseHash i = final
  where
    (final, pos, steps) = runMultiSteps [0..255] i 0 0 64

getDenseHash :: [[Bool]] -> [[Bool]]
getDenseHash [] = []
getDenseHash hash = (xorBitsList (take 16 hash)) : (getDenseHash (drop 16 hash))

to8Bits :: Int -> [Bool]
to8Bits i = iToBits i 128

iToBits :: Int -> Int -> [Bool]
iToBits i 1 = [((div i 1)==1)]
iToBits i b = (d == 1):(iToBits (i - (d * b)) (div b 2))
  where
    d = div i b


xor :: Bool -> Bool -> Bool
xor x False = x
xor x True = not x

xorBits :: [Bool] -> [Bool] -> [Bool]
xorBits [] [] = []
xorBits (v:vs) (w:ws) = (xor v w):(xorBits vs ws)

xorBitsList :: [[Bool]] -> [Bool]
xorBitsList l = foldr1 xorBits l

bitsToHex :: [Bool] -> [Char]
bitsToHex [] = []
bitsToHex l = (getHexValue (take 4 l)):(bitsToHex (drop 4 l))

getHexValue :: [Bool] -> Char
getHexValue l
  | ((l!!0) == False) && ((l!!1) == False) && ((l!!2) == False) && ((l!!3) == False) = '0'
  | ((l!!0) == False) && ((l!!1) == False) && ((l!!2) == False) && ((l!!3) == True) = '1'
  | ((l!!0) == False) && ((l!!1) == False) && ((l!!2) == True) && ((l!!3) == False) = '2'
  | ((l!!0) == False) && ((l!!1) == False) && ((l!!2) == True) && ((l!!3) == True) = '3'
  | ((l!!0) == False) && ((l!!1) == True) && ((l!!2) == False) && ((l!!3) == False) = '4'
  | ((l!!0) == False) && ((l!!1) == True) && ((l!!2) == False) && ((l!!3) == True) = '5'
  | ((l!!0) == False) && ((l!!1) == True) && ((l!!2) == True) && ((l!!3) == False) = '6'
  | ((l!!0) == False) && ((l!!1) == True) && ((l!!2) == True) && ((l!!3) == True) = '7'
  | ((l!!0) == True) && ((l!!1) == False) && ((l!!2) == False) && ((l!!3) == False) = '8'
  | ((l!!0) == True) && ((l!!1) == False) && ((l!!2) == False) && ((l!!3) == True) = '9'
  | ((l!!0) == True) && ((l!!1) == False) && ((l!!2) == True) && ((l!!3) == False) = 'a'
  | ((l!!0) == True) && ((l!!1) == False) && ((l!!2) == True) && ((l!!3) == True) = 'b'
  | ((l!!0) == True) && ((l!!1) == True) && ((l!!2) == False) && ((l!!3) == False) = 'c'
  | ((l!!0) == True) && ((l!!1) == True) && ((l!!2) == False) && ((l!!3) == True) = 'd'
  | ((l!!0) == True) && ((l!!1) == True) && ((l!!2) == True) && ((l!!3) == False) = 'e'
  | ((l!!0) == True) && ((l!!1) == True) && ((l!!2) == True) && ((l!!3) == True) = 'f'

                                      
puzzle2 :: [Char]
puzzle2 = foldr1 (++) (map bitsToHex (getDenseHash (map to8Bits (getSparseHash input2))))
 
