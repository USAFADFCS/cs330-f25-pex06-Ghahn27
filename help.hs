typeOne :: [(Char, Char)] -> Bool
typeOne [] = False
typeOne (x:xs) = 
   if null xs then False
   else if fst x == fst (head xs) then True
   else typeOne xs

-- with in list if (a,o) (b,o) ... (a,u) (b,u) x1 != x2 & y1 = y2 & x3 != x4 & y3 = y4 & x1 = x3 & x2 = x4 so
-- no need for != so y1 = y2 & y3 = y4 & x1 = x3 & x2 = x4

matchesPair :: (Char, Char) -> (Char, Char) -> (Char, Char) -> (Char, Char) -> Bool
matchesPair (x1,y1) (x2,y2) (x3,y3) (x4,y4) =
  ((x1 == x3) && (x2 == x4) && (y1 == y2) && (y3 == y4)) ||
  ((x1 == x4) && (x2 == x3) && (y1 == y3) && (y2 == y4))

--search thru list for two type one pairs, either x or y. Need || for y pairs in matchespAir

typeTwoPair :: (Char, Char) -> (Char, Char) -> [(Char, Char)] -> Bool
typeTwoPair p1 p2 [] = False
typeTwoPair p1 p2 (x:xs) = 
   if null xs then False
   else if matchesPair p1 p2 x (head xs) then True
   else  typeTwoPair p1 p2 xs


typeTwo :: [(Char, Char)] -> Bool
typeTwo [] = False 
typeTwo (x:xs) = 
   if null xs then False
   else if null (tail xs) then False
   else typeTwoPair x (head xs) (tail xs)

