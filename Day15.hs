

sampleSize :: Integer
sampleSize = 40000000
sampleSize2 :: Integer
sampleSize2 = 5000000

genAFactor :: Integer
genAFactor = 16807

genAInit :: Integer
genAInit = 699
  
genBFactor :: Integer
genBFactor = 48271

genBInit :: Integer
genBInit = 124

genMod :: Integer
genMod = 2147483647

genANext :: Integer -> Integer
genANext n = mod (n * genAFactor) genMod

genBNext :: Integer -> Integer
genBNext n = mod (n * genBFactor) genMod

genANext2 :: Integer -> Integer
genANext2 n = if (mod nextA 4)==0 then nextA else genANext2 nextA
  where nextA = mod (n * genAFactor) genMod

genBNext2 :: Integer -> Integer
genBNext2 n = if (mod nextB 8)==0 then nextB else genBNext2 nextB
  where nextB = mod (n * genBFactor) genMod




toBits :: Integer -> Integer -> [Bool]
toBits i j = iToBits i (2^(j-1))

iToBits :: Integer -> Integer -> [Bool]
iToBits i 1 = [((div i 1)==1)]
iToBits i b = (d == 1):(iToBits (i - (d * b)) (div b 2))
  where
    d = div i b

compareLowerBits :: Integer -> Integer -> Bool
compareLowerBits n m = (drop 16 (toBits n 32)) == (drop 16 (toBits m 32))

runSamples :: Integer -> Integer -> Integer -> Integer -> Integer
runSamples 0 c _ _= c
runSamples i c a b = if (compareLowerBits nexta nextb) then runSamples (i - 1) (c + 1) nexta nextb else runSamples (i - 1) c nexta nextb
  where
    nexta = genANext a
    nextb = genBNext b

runSamples2 :: Integer -> Integer -> Integer -> Integer -> Integer
runSamples2 0 c _ _= c
runSamples2 i c a b = if (compareLowerBits nexta nextb) then runSamples2 (i - 1) (c + 1) nexta nextb else runSamples2 (i - 1) c nexta nextb
  where
    nexta = genANext2 a
    nextb = genBNext2 b

