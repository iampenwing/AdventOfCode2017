import Day5Input

step :: (Int, [Int]) -> (Int, [Int])
step (n, []) = (-1, [])
step (n, (x:xs)) = ((n + (x:xs)!!n), ((take (n) (x:xs)) ++ [(((x:xs)!!n)+1)] ++ (drop n xs)))

escape :: (Int, [Int]) -> Int -> Int
escape (x, l) n
  | (x < 0) || (x >= length l) = n
  | otherwise = escape (step (x, l)) (n + 1)


part2step :: (Int, [Int]) -> (Int, [Int])
part2step (n, []) = (-1, [])
part2step (n, (x:xs)) 
  | (x:xs)!!n >= 3 = ((n + (x:xs)!!n), ((take (n) (x:xs)) ++ [(((x:xs)!!n)-1)] ++ (drop n xs)))
  | otherwise = ((n + (x:xs)!!n), ((take (n) (x:xs)) ++ [(((x:xs)!!n)+1)] ++ (drop n xs)))

part2escape :: (Int, [Int]) -> Int -> Int
part2escape (x, l) n
  | (x < 0) || (x >= length l) = n
  | otherwise = part2escape (part2step (x, l)) (n + 1)

part2escape2 :: (Int, [Int]) -> Int -> ((Int, [Int]), Int)
part2escape2 (x, l) n
  | (x < 0) || (x >= length l) = ((x, l), n)
  | n > 1000000000 = ((x, l), n)
  | otherwise = part2escape2 (part2step (x, l)) (n + 1)

