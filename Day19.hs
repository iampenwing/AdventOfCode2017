import Day19Input

findInput :: [Char] -> Int -> (Int, Int)
findInput [] x = (x, 0)
findInput ('|':_) x = (x, 0)
findInput (row:rows) x = findInput rows (x + 1)

followPath :: [[Char]] -> (Int, Int) -> Int -> Char -> [Char] -> Int -> ([Char], Int)
followPath maze (x, y) direction current letters count
  | (direction == 0) && ((current == '|') || (current == '-')) = followPath maze (x, (y+1)) direction down letters (count + 1)
  | (direction == 1) && ((current == '|') || (current == '-')) = followPath maze ((x+1), y) direction right letters (count + 1)
  | (direction == 2) && ((current == '|') || (current == '-')) = followPath maze (x, (y-1)) direction up letters (count + 1)
  | (direction == 3) && ((current == '|') || (current == '-')) = followPath maze ((x-1), y) direction left letters (count + 1)
  | (direction == 0) && (current == '+') && (x == ((length (maze!!0)) - 1)) = followPath maze ((x-1), y) 3 left letters (count + 1)
  | (direction == 1) && (current == '+') && (y == ((length maze) - 1)) = followPath maze (x, (y-1)) 2 up letters (count + 1)
  | (direction == 2) && (current == '+') && (x == 0) = followPath maze ((x+1), y) 1 right letters (count + 1)
  | (direction == 3) && (current == '+') && (y == 0) = followPath maze (x, (y+1)) 0 down letters (count + 1)
  | (direction == 0) && (current == '+') && (left == '-') = followPath maze ((x-1), y) 3 left letters (count + 1)
  | (direction == 0) && (current == '+') && (right == '-') = followPath maze ((x+1), y) 1 right letters (count + 1)
  | (direction == 1) && (current == '+') && (up == '|') = followPath maze (x, (y-1)) 2 up letters (count + 1)
  | (direction == 1) && (current == '+') && (down == '|') = followPath maze (x, (y+1)) 0 down letters (count + 1)
  | (direction == 2) && (current == '+') && (left == '-') = followPath maze ((x-1), y) 3 left letters (count + 1)
  | (direction == 2) && (current == '+') && (right == '-') = followPath maze ((x+1), y) 1 right letters (count + 1)
  | (direction == 3) && (current == '+') && (up == '|') = followPath maze (x, (y-1)) 2 up letters (count + 1)
  | (direction == 3) && (current == '+') && (down == '|') = followPath maze (x, (y+1)) 0 down letters (count + 1)
  | (direction == 0) && ((current >= 'A') && (current <= 'Z')) = followPath maze (x, (y+1)) direction down (letters++[current]) (count + 1)
  | (direction == 1) && ((current >= 'A') && (current <= 'Z')) = followPath maze ((x+1), y) direction right (letters++[current]) (count + 1)
  | (direction == 2) && ((current >= 'A') && (current <= 'Z')) = followPath maze (x, (y-1)) direction up (letters++[current]) (count + 1)
  | (direction == 3) && ((current >= 'A') && (current <= 'Z')) = followPath maze ((x-1), y) direction left (letters++[current]) (count + 1)
  | (current == ' ') = (letters, count)
  where
    left = if (x==0) then '.' else ((maze!!y)!!(x-1))
    right = if (x > (length (maze!!0))) then '.' else ((maze!!y)!!(x+1))
    up = if (y==0) then '.' else ((maze!!(y-1))!!x)
    down = if (y > (length maze)) then '.' else ((maze!!(y+1))!!x)

  

