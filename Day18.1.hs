import Day18Input

getVal :: [(Char, Int)] -> Char -> Int
getVal [] _ = 0
getVal ((xr, xv):xs) y
  | (xr == y) = xv
  | otherwise = getVal xs y

setVal :: [(Char, Int)] -> Char -> Int -> [(Char, Int)]
setVal [] reg val = [(reg, val)]
setVal ((xreg, xval):registers) reg val
  | (xreg == reg) = (reg, val) : registers
  | otherwise = (xreg, xval) : setVal registers reg val

interpretInstruction :: Instruction -> [(Char, Int)] -> (Int, Char, Int)
interpretInstruction (Snd x) _ = (0, '.', x)
interpretInstruction (Sndr x) registers = (0, '.', (getVal registers x))
interpretInstruction (Set (x, y)) _ = (1, x, y)
interpretInstruction (Setr (x, y)) registers = (1, x, (getVal registers y))
interpretInstruction (Add (x, y)) _ = (2, x, y)
interpretInstruction (Addr (x, y)) registers = (2, x, (getVal registers y))
interpretInstruction (Mul (x, y)) _ = (3, x, y)
interpretInstruction (Mulr (x, y)) registers = (3, x, (getVal registers y))
interpretInstruction (Mod (x, y)) _ = (4, x, y)
interpretInstruction (Modr (x, y)) registers = (4, x, (getVal registers y))
interpretInstruction (Rcv x) registers = (5, x, (getVal registers x))
interpretInstruction (Jgz (x, y)) _ = (6, x, y)
interpretInstruction (Jgzr (x, y)) registers = (6, x, (getVal registers y))
interpretInstruction (Jgzi (x, y)) _
  | (x>0) = (7, '.', y)
  | otherwise = (8, '.', y)

runInstructions :: [Instruction] -> ([(Char, Int)], [Int], Int) -> ([(Char, Int)], [Int], Int) -> Int -> Int
runInstructions instructions (registersA, queueA, posA) (registersB, queueB, posB) countA
  | (insA == 0) && (insB == 0) = runInstructions instructions (registersA, (queueA ++ [valB]), (posA + 1)) (registersB, (queueB ++ [valA]), (posB + 1)) (countA + 1)
  | (insA == 0) && (insB == 5) && (queueB /= []) = runInstructions instructions (registersA, queueA, (posA + 1)) ((setVal registersB regB (head queueB)), (tail queueB), (posB + 1)) (countA + 1)
  | (insA == 0) && (insB == 5) && (queueB == []) = runInstructions instructions (registersA, queueA, (posA + 1)) (registersB, [valA], posB) (countA + 1)

  | (insA == 0) && (insB /= 0) && (insB /= 5) = runInstructions instructions (registersA, queueA, (posA + 1)) (newRegB, (newQueueB ++ [valA]), newPosB) (countA + 1)
  
  | (insA == 5) && (insB == 0) && (queueA /= []) = runInstructions instructions ((setVal registersA regA (head queueA)), (tail queueA), (posA + 1)) (registersB, queueB, (posB + 1)) countA
  | (insA == 5) && (insB == 0) && (queueA == []) = runInstructions instructions (registersA, [valB], posA) (registersB, queueB, (posB + 1)) countA
  | (insA /= 0) && (insA /= 5) && (insB == 0) = runInstructions instructions (newRegA, (newQueueA ++ [valB]), newPosA) (registersB, queueB, (posB + 1)) countA
  | (insA == 5) && (insB == 5) && (queueA == []) && (queueB == []) = countA
  | (insA == 5) && (insB == 5) && (queueA == []) && (queueB /= []) = runInstructions instructions (registersA, queueA, posA) ((setVal registersB regB (head queueB)), (tail queueB), (posB + 1)) countA
  | (insA == 5) && (insB == 5) && (queueA /= []) && (queueB == []) = runInstructions instructions ((setVal registersA regA (head queueA)), (tail queueA), (posA + 1)) (registersB, queueB, posB) countA
  | (insA == 5) && (insB == 5) && (queueA /= []) && (queueB /= []) = runInstructions instructions ((setVal registersA regA (head queueA)), (tail queueA), (posA + 1)) ((setVal registersB regB (head queueB)), (tail queueB), (posB + 1)) countA
  | (insA == 5) && (queueA == []) && (insB /= 0) && (insB /= 5) = runInstructions instructions (registersA, queueA, posA) (newRegB, newQueueB, newPosB) countA
  | (insA == 5) && (queueA /= []) && (insB /= 0) && (insB /= 5) = runInstructions instructions ((setVal registersA regA (head queueA)), (tail queueA), (posA + 1)) (newRegB, newQueueB, newPosB) countA
  | (insB == 5) && (queueB == []) && (insA /= 0) && (insA /= 5) = runInstructions instructions (newRegA, newQueueA, newPosA) (registersB, queueB, posB) countA
  | (insB == 5) && (queueB /= []) && (insA /= 0) && (insA /= 5) = runInstructions instructions (newRegA, newQueueA, newPosA) ((setVal registersB regB (head queueB)), (tail queueB), (posB + 1)) countA
  | otherwise = runInstructions instructions (newRegA, newQueueA, newPosA) (newRegB, newQueueB, newPosB) countA
  where
    (insA, regA, valA) = interpretInstruction (instructions!!posA) registersA
    (insB, regB, valB) = interpretInstruction (instructions!!posB) registersB
    (newRegA, newQueueA, newPosA) = runInstruction (insA, regA, valA) (registersA, queueA, posA)
    (newRegB, newQueueB, newPosB) = runInstruction (insB, regB, valB) (registersB, queueB, posB)

-- This just ran and ran and ran...
runInstruction :: (Int, Char, Int) -> ([(Char, Int)], [Int], Int) -> ([(Char, Int)], [Int], Int)
runInstruction (ins, reg, val) (registers, queue, pos)
  | (ins == 1) = ((setVal registers reg val), queue, (pos + 1))
  | (ins == 2) = ((setVal registers reg ((getVal registers reg) + val)), queue, (pos + 1))
  | (ins == 3) = ((setVal registers reg ((getVal registers reg) * val)), queue, (pos + 1))
  | (ins == 4) = ((setVal registers reg (mod (getVal registers reg) val)), queue, (pos + 1))
  | (ins == 6) && ((getVal registers reg) > 0) = (registers, queue, (pos + val))
  | (ins == 6) && ((getVal registers reg) <= 0) = (registers, queue, (pos + 1))
  | (ins == 7) = (registers, queue, (pos + val))
  | (ins == 8) = (registers, queue, (pos + 1))
--  | otherwise = (registers, queue, pos)


-- So I made this following the "can run at different speeds" 'hint'...
-- Technically this cheats as I don't know, on return which is the original Machine A and which is
-- the original Machine B so I have to try each one...
runInstructions2 :: [Instruction] -> ([(Char, Int)], [Int], Int, Int) -> ([(Char,Int)], [Int], Int, Int) -> (Int,Int)
runInstructions2 instructions machineA machineB
  | (insA == 0) = runInstructions2 instructions (regsA, queueA, (posA+1), (countA+1)) (regsB, (queueB ++ [valA]), posB, countB)
  | (insA == 5) && (queueA /= []) = runInstructions2 instructions ((setVal regsA regA (head queueA)), (tail queueA), (posA + 1), countA) machineB
  | (insA == 5) && (queueA == []) && (insB == 5) && (queueB == []) = (countA, countB)
  | (insA == 5) && (queueA == []) && (((insB == 5) && (queueB /= [])) || insB /= 5) = runInstructions2 instructions machineB machineA
  | otherwise = runInstructions2 instructions (newRegsA, newQueueA, newPosA, countA) machineB
  where
    (regsA, queueA, posA, countA) = machineA
    (regsB, queueB, posB, countB) = machineB
    (insA, regA, valA) = interpretInstruction (instructions!!posA) regsA
    (insB, regB, valB) = interpretInstruction (instructions!!posB) regsB
    (newRegsA, newQueueA, newPosA) = runInstruction (insA, regA, valA) (regsA, queueA, posA)
