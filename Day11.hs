import Day11Input

followPath :: [[Char]] -> (Int, Int) -> (Int, Int)
followPath [] x = x
followPath (route:routes) (ns, ew)
  | route == "n" = followPath routes ((ns+2), ew)
  | route == "s" = followPath routes ((ns-2), ew)
  | route == "nw" = followPath routes ((ns+1), (ew-1))
  | route == "ne" = followPath routes ((ns+1), (ew+1))
  | route == "sw" = followPath routes ((ns-1), (ew-1))
  | route == "se" = followPath routes ((ns-1), (ew+1))

followPath2 :: [[Char]] -> (Int, Int) -> Int -> Int
followPath2 [] _ x = x
followPath2 (route:routes) (ns, ew) d
  | route == "n" = followPath2 routes ((ns+2), ew) (max d (getDistance ((ns+2), ew)))
  | route == "s" = followPath2 routes ((ns-2), ew) (max d (getDistance ((ns-2), ew)))
  | route == "nw" = followPath2 routes ((ns+1), (ew-1)) (max d (getDistance ((ns+1), (ew-1))))
  | route == "ne" = followPath2 routes ((ns+1), (ew+1)) (max d (getDistance ((ns+1), (ew+1))))
  | route == "sw" = followPath2 routes ((ns-1), (ew-1)) (max d (getDistance ((ns-1), (ew-1))))
  | route == "se" = followPath2 routes ((ns-1), (ew+1)) (max d (getDistance ((ns-1), (ew+1))))

getDistance :: (Int, Int) -> Int
getDistance (ns, ew) = (abs ew) + (div ((abs ns) - (abs ew)) 2)

