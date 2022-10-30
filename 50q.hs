--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo'  x y 
            | x > y = []
            | x <= y = x : enumFromTo' (x+1) y 

--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo'  x n y 
               | x > y = []
               | n > y = [x]
               | otherwise = x : enumFromThenTo' n (2*n-x) y

--3
joinList :: [a] -> [a] -> [a]
--joinList [] [] = []
joinList [] l = l
joinList l [] = l
joinList (h:t) l = h : joinList t l

--4
pos :: [a] -> Int -> a
pos (h:t) 0 = h
pos (h:t) n = pos t (n-1)

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

--6
take' :: Int -> [a] -> [a]
take' 0 l = []
take' n (h:t) = h : take' (n-1) t 

--7
drop' :: Int -> [a] -> [a]
drop' 0 l = l 
drop' _ [] = []
drop' n (h:t) = drop' (n-1) t 

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys  

--9
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x 

--10
intersperce' :: a -> [a] -> [a]
intersperce' s [] = []
intersperce' s [x] = [x]  
intersperce' s (h:t) = h : s : intersperce' s t     

--11
-- group' :: Eq a => [a] -> [[a]]
-- group' [] = []
-- group' (h:t) = takewhile' h t : group (dropwhile' h (h:t))

-- takewhile' :: Eq a => [a] -> [a]
-- takewhile' a [] = [a]
-- takewhile' a (h:t)  | a == h = h : takewhile' a t 
--                     | otherwise = [a]

-- dropwhile' :: Eq a => a -> [a] -> [a]
-- dropwhile' a [] = []
-- dropwhile' a (h:t) | a == h = dropwhile' a t 
--                    | otherwise = h:t

group' :: Eq a => [a] -> [[a]] 
group' [] = []
group' [h] = [[h]] 
group' (h:t) = let ((x:xs) : y) = group' t 
               in if h == x then (h:x:xs) : y
               else [h] : (x:xs) : y
 
--12
concat' :: [[a]] -> [a] 
concat' [] = []
concat' (h:t) = h ++ concat' t 

--13
inits' :: [a] -> [[a]] 
inits' l = initsAux ( reverse' l )

-- [3,2,1] -> [[],[1],[1,2],                [1,2,3]]
initsAux :: [a] -> [[a]] 
initsAux [] = [] 
initsAux (h:t) = initsAux t ++ [reverse' (h:t)]     


--14
tails' :: [a] -> [[a]]
tails' [] = []
tails' (h:t) = (h:t) : tails' t 

--15
heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t
heads' ((x:xs):t) = x : heads' t  

--16
total' :: [[a]] -> Int 
total' [] = 0 
total' (h:t) = length h + total' t

--17 
fun' :: [(a,b,c)] -> [(a,c)] 
fun' [] = []
fun' ((x,y,z):t) = (x,z) : fun' t

--18
cola' :: [(String,b,c)] -> String
cola' [] = [] 
cola' ((x,_,_):t) = x ++ cola' t 

--19
idade' :: Int -> Int -> [(String,Int)] -> [String] 
idade' _ _ [] = [] 
idade' year age ((n, b):t) | year - b >= age = n : idade' year age t
                           | otherwise = idade' year age t 

--20
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' _ 0 = []
powerEnumFrom' n e = powerEnumFrom' n (e-1) ++ [n^(e-1)] 

--21
isPrime' :: Int -> Bool
isPrime' n = primeAux' n (floor $ sqrt $ fromIntegral n)

primeAux' :: Int -> Int -> Bool
primeAux' n 2 = mod n 2 /= 0 
primeAux' n m | mod n m == 0 = False
              | otherwise = primeAux' n (m-1) 

--22
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) | x == y = isPrefixOf' xs ys 
                          | otherwise = False 

--23
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l1 l2 | (last l2) == (last l1) = isSuffixOf' (init l1) (init l2)
                   | otherwise = False

--24
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) | x == y = isSubsequenceOf' xs ys
                               | otherwise = isSubsequenceOf' (x:xs) ys

--25
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x l = elemIndicesAux' x l 0

elemIndicesAux' :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux' x [] _ = []
elemIndicesAux' x (h:t) p | x == h = p : elemIndicesAux' x t (p+1)
                          | otherwise = elemIndicesAux' x t (p+1) 

--26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = h : nub' (nubAux h t)

nubAux :: Eq a => a -> [a] -> [a]
nubAux _ [] = []
nubAux x (y:ys) | x == y    = nubAux x ys
                | otherwise = y : nubAux x ys

--27
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (h:t) | x == h = t
                | otherwise = h : delete' x t 

--28
remove' :: Eq a => [a] -> [a] -> [a]
remove' l [] = l 
remove' [] _ = [] 
remove' (x:xs) (y:ys) | x == y    = remove' xs ys 
                      | otherwise = remove' xs ys 

--29
union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l 
union' l [] = l 
union' l (y:ys) | unionAux l y == True = union' l ys
                | otherwise = union' (l ++ [y]) ys

unionAux :: Eq a => [a] -> a -> Bool
unionAux [] x = False
unionAux (h:t) x | x == h = True
                 | otherwise = unionAux t x

--30
-- intersect' :: Eq a => [a] -> [a] -> [a]
-- intersect' [] [] = [] 
-- intersect' [] l = [] 
-- intersect' l [] = []
-- intersect' (h:t) l | h  

--31
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) | x > h = h : insert' x t 
                | otherwise = x : h : t

--32
unwords' :: [String] -> String
unwords' [] = ""
unwords' [a] = a
unwords' (h:t) = h ++ " " ++ unwords' t 
--how to space this shit? 2 words aint spaced

--33
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t 

--34
pMaior' :: Ord a => [a] -> Int
pMaior' (h:t) = pMaiorAux t h 0 1
 
pMaiorAux :: Ord a => [a] -> a -> Int -> Int -> Int
pMaiorAux [] m pm pa = pm
pMaiorAux (h:t) m pm pa | h > m = pMaiorAux t h pa (pa+1)
                        | otherwise = pMaiorAux t m pm (pa+1)

--35
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ((a,b):t) | x == a    = Just b 
                    | otherwise = lookup' x t 

--36
preCrescente' :: Ord a => [a] -> [a]
preCrescente' [] = [] 
preCrescente' [x] = [x] 
preCrescente' (x:y:z) | x <= y    = x : preCrescente' (y:z) 
                      | otherwise = [x] 

--37
iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:t) = insert2 h (iSort' t)

insert2 :: Ord a => a -> [a] -> [a]
insert2 x [] = [x]
insert2 x (h:t) | x <= h    = x : h : t 
                | otherwise = h : insert2 x t 

--38
menor' :: String -> String -> Bool
menor' [] _ = True
menor' _ [] = False
menor' (x:xs) (y:ys) | x < y = True
                     | x > y = False 
                     | otherwise = menor' xs ys 

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
















