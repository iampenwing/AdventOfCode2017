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

runInstructions :: [Instruction] -> [(Char, Int)] -> Int -> Int -> Int
runInstructions instructions registers last pos
  | (ins == 0) = runInstructions instructions registers val (pos + 1)
  | (ins == 1) = runInstructions instructions (setVal registers reg val) last (pos + 1)
  | (ins == 2) = runInstructions instructions (setVal registers reg ((getVal registers reg) + val)) last (pos + 1)
  | (ins == 3) = runInstructions instructions (setVal registers reg ((getVal registers reg) * val)) last (pos + 1)
  | (ins == 4) = runInstructions instructions (setVal registers reg (mod (getVal registers reg) val)) last (pos + 1)
  | (ins == 5) && (val == 0) = runInstructions instructions registers last (pos + 1)
  | (ins == 5) && (val /= 0) = last
  | (ins == 6) && ((getVal registers reg) > 0) = runInstructions instructions registers last (pos + val)
  | (ins == 6) && ((getVal registers reg) <= 0) = runInstructions instructions registers last (pos + 1)
  | (ins == 7) = runInstructions instructions registers last (pos + val)
  | (ins == 8) = runInstructions instructions registers last (pos + 1)
  where
    (ins, reg, val) = interpretInstruction (instructions!!pos) registers
