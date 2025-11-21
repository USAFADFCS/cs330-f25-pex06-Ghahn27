-- pex6.hs 
-- unKnot Haskell

-- name: Garrett Hahn

{- DOCUMENTATION: C1C Ramsey helped me understand the concept of the tripCode and why it was neccessary to be a string. Also the fact that I have to remove
pairs after a move is completed.
-}




typeOne :: [(Char, Char)] -> Bool
typeOne [] = False
typeOne (x:xs) = 
   if null xs then False
   else if fst x == fst (head xs) then True
   else typeOne xs

typeOneDo :: [(Char, Char)] -> [(Char, Char)]
typeOneDo [] = [] 
typeOneDo (x:xs) = 
   if null xs then [x]
   else if fst x == fst (head xs) then tail xs
   else x : typeOneDo xs

-- with in list if (a,o) (b,o) ... (a,u) (b,u) x1 != x2 & y1 = y2 & x3 != x4 & y3 = y4 & x1 = x3 & x2 = x4 so
-- no need for != so y1 = y2 & y3 = y4 & x1 = x3 & x2 = x4

matchesPair :: (Char, Char) -> (Char, Char) -> (Char, Char) -> (Char, Char) -> Bool
matchesPair (x1,y1) (x2,y2) (x3,y3) (x4,y4) =
  ((x1 == x3) && (x2 == x4) && (y1 /= y3) && (y2 /= y4)) ||
  ((x1 == x4) && (x2 == x3) && (y1 /= y4) && (y2 /= y3))

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


typeTwoRemove :: (Char, Char) -> (Char, Char) -> [(Char, Char)] -> [(Char, Char)]
typeTwoRemove p1 p2 [] = []
typeTwoRemove p1 p2 (x:xs) = 
   if null xs then [x]
   else if matchesPair p1 p2 x (head xs) then tail xs
   else x : typeTwoRemove p1 p2 xs

typeTwoDo :: [(Char, Char)] -> [(Char , Char)] 
typeTwoDo [] = []
typeTwoDo (x:xs) = 
   if null xs then [x]
    else typeTwoRemove x (head xs) (tail xs)

tripCodeWrap :: [(Char, Char)] -> [(Char, Char)]
tripCodeWrap [] = []
tripCodeWrap tc = tail tc ++ [head tc]

-- if knot cant keep wrapping cuz infinite
tripCodeStop :: [(Char, Char)] -> [(Char, Char)] -> Bool
tripCodeStop [] [] = True
tripCodeStop [] _ = False
tripCodeStop _ [] = False
tripCodeStop (x:xs) (y:ys) = if x == y then tripCodeStop xs ys else False

unKnotHelper :: [(Char, Char)] -> [(Char, Char)] -> Bool -> String
unKnotHelper [] _ _ = "not a knot"
unKnotHelper tripCode original isFirst
  | typeOne tripCode = unKnotHelper (typeOneDo tripCode) (typeOneDo tripCode) True
  | typeTwo tripCode = unKnotHelper (typeTwoDo tripCode) (typeTwoDo tripCode) True
  | (not isFirst) && tripCodeStop tripCode original = "tangle - resulting trip code: " ++ show tripCode
  | otherwise = unKnotHelper (tripCodeWrap tripCode) original False

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | otherwise = unKnotHelper tripCode tripCode True


main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

   let t02 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("   test case t02 - tripcode: " ) -- "Unkot"
   print(t02)
   print("   result:" ++ unKnot t02) -- "Unkot"
 
   let t03 = [('a','u'),('b','u'),('a','o'),('b','o')]
   print("   test case t03 - tripcode: " )
   print(t03)
   print("   result:" ++ unKnot t03) -- "Unkot"
 
   let t04 = [('a','o'),('b','u'),('a','u'),('b','o')]
   print("   test case t04 - tripcode: " )
   print(t04)
   print("   result:" ++ unKnot t04) -- "Unkot"

   let t06 = [('a','o'),('q','u'), ('a','u')]
   print("   test case t06 - tripcode: " )
   print(t06)
   print("   result:" ++ unKnot t06) -- [('q','u')]
 
   let t07 = [('a','o'),('a','u'), ('q','u')]
   print("   test case t07 - tripcode: " )
   print(t07)
   print("   result:" ++ unKnot t07) -- [('q','u')]
 
   let t08 = [('a','o'),('b','o'), ('a','u'), ('b','u'),('q','u')]
   print("   test case t08 - tripcode: " )
   print(t08)
   print("   result:" ++ unKnot t08) -- [('q','u')]
 
   let t09 = [('a','u'),('b','o'), ('a','o'), ('b','u'),('q','u')]
   print("   test case t09 - tripcode: " )
   print(t09)
   print("   result:" ++ unKnot t09) -- [('a','o'),('b','o'), ('a','u'), ('b','u') ('q','u')]
 
   let t10 = [('a','u'),('b','o'), ('a','o'), ('b','u'),('q','u'),('c','o'),('c','u')]
   print("   test case t10 - tripcode: " )
   print(t10)
   print("   result:" ++ unKnot t10) -- [('a','o'),('b','o'), ('a','u'), ('b','u') ('q','u')]
 
   let t11 = [('a','u'),('b','o'), ('a','o'), ('q','u'),('b','u'),('c','o'),('c','u')]
   print("   test case t11 - tripcode: " )
   print(t11)
   print("   result:" ++ unKnot t11) -- [('q','u')]
 
   let t12 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('q','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t12 - tripcode: " )
   print(t12)
   print("   result:" ++ unKnot t12) -- [('q','u')]