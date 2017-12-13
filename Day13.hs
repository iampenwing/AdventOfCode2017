import Day13Input

runFirewall :: [(Int, Int)] -> Int -> Int -> Int -> Int
runFirewall [] s _ _ = s
runFirewall ((lev,dep):fws) s l d
  | (l==lev) && (mod (l+d) (2 * (dep - 1)) == 0) = runFirewall fws (s + (lev * dep)) (l+1) d
  | (l==lev) && (mod (l+d) (2 * (dep - 1)) /= 0) = runFirewall fws s (l+1) d
  | otherwise = runFirewall ((lev,dep):fws) s (l+1) d

safeRunFirewall :: [(Int, Int)] -> Int -> Int
safeRunFirewall fw delay
  | (s == 0) && ((mod delay (2 * (dep - 1))) /= 0) = delay
  | otherwise = safeRunFirewall fw (delay + 1)
  where
    s = runFirewall fw 0 0 delay
    (_, dep) = head fw

safeRun2 :: [(Int, Int)] -> Int -> Int
safeRun2 fw delay
  | foldr1 (||) (map (\x -> (mod (delay + (fst x)) (2 * ((snd x) - 1))) == 0) fw) = safeRun2 fw (delay + 1)
  | otherwise = delay
  
