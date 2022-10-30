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














