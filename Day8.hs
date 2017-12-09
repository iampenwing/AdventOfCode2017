import Day8Input

modifyReg :: [([Char], Int)] -> [Char] -> (Int->Int) -> [([Char],Int)]
modifyReg [] reg f = [(reg, (f 0))]
modifyReg ((creg, cval):regs) reg f
  | creg == reg = (creg, (f cval)):regs
  | otherwise = (creg, cval):(modifyReg regs reg f)

runInstruction :: [([Char], Int)] -> ([Char], (Int -> Int), ([Char], Int, Int)) -> [([Char],Int)]
runInstruction regs (regToMod, f, (regToTest, typeOfTest, valOfTest))
  | (typeOfTest == 1) && ((getRegVal regs regToTest) < valOfTest) = modifyReg regs regToMod f
  | (typeOfTest == 2) && ((getRegVal regs regToTest) > valOfTest) = modifyReg regs regToMod f
  | (typeOfTest == 3) && ((getRegVal regs regToTest) >= valOfTest) = modifyReg regs regToMod f
  | (typeOfTest == 4) && ((getRegVal regs regToTest) <= valOfTest) = modifyReg regs regToMod f
  | (typeOfTest == 5) && ((getRegVal regs regToTest) == valOfTest) = modifyReg regs regToMod f
  | (typeOfTest == 6) && ((getRegVal regs regToTest) /= valOfTest) = modifyReg regs regToMod f
  | otherwise = regs
  
runInstructions :: [([Char], Int)] -> [([Char], (Int -> Int), ([Char], Int, Int))] -> [([Char],Int)]
runInstructions regs [] = regs
runInstructions regs (instruction:instructions) = runInstructions (runInstruction regs instruction) instructions 

findLargestVal :: [([Char], Int)] -> Int -> Int
findLargestVal [] val = val
findLargestVal ((reg, val):regs) seed
  | seed > val = findLargestVal regs seed
  | otherwise = findLargestVal regs val

runInstructionsHighVal :: [([Char], Int)] -> [([Char], (Int -> Int), ([Char], Int, Int))] -> Int -> Int
runInstructionsHighVal regs [] highVal = highVal
runInstructionsHighVal regs (instruction:instructions) highVal
  = runInstructionsHighVal newRegs instructions (findLargestVal newRegs highVal)
  where
    newRegs = runInstruction regs instruction

