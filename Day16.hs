
import Day16Input

repeats :: Integer
repeats = 1000000000

partner :: [Char] -> Char -> Char -> [Char]
partner [] _ _ = []
partner (x:xs) a b
  | (x==a) = b:(partner xs a b)
  | (x==b) = a:(partner xs a b)
  | otherwise = x:(partner xs a b)

dance :: [Char] -> [Move] -> [Char]
dance origin [] = origin
dance origin (move:moves) = dance (step origin move) moves

step :: [Char] -> Move -> [Char]
step origin (Spin a) = (drop ((length origin) - a) origin) ++ (take ((length origin) - a) origin) 
step origin (Partner (a, b)) = partner origin a b
step origin (Exchange (a, b)) = (take aa origin) ++ [origin!!ab] ++ (drop (aa+1) (take ab origin)) ++ [origin!!aa] ++ (drop (ab+1) origin)
  where
    aa = min a b
    ab = max a b


multiDance :: [Char] -> [Move] -> Integer -> [Char]
multiDance origin _ 0 = origin
multiDance origin moves count = multiDance (dance origin moves) moves (count - 1)


