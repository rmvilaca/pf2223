--ficha 2 
--4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

p :: Polinomio
p = [(2,3), (3,4), (5,3), (4,5)]

--a
conta :: Int -> Polinomio -> Int
conta n  [] = 0
conta n ((c,e) : t ) | n == e    = 1 + conta n t 
                     | otherwise = conta n t 

--b
grau :: Polinomio -> Int
grau [(c,e)] = e 
grau ((c,e) : t) = max e (grau t)  

--c
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = [] 
selgrau  n ((c,e) : t) = if (n == e) then (c,e) : selgrau n t 
                         else selgrau n t 

--d
deriv :: Polinomio -> Polinomio 
deriv [] = []
deriv ((c,e) : t) | (e == 0) =  deriv t
                  | otherwise = (c*(fromIntegral e),e-1) : deriv t

--e
calcula :: Float -> Polinomio -> Float
calcula n [(c,e)]     = c*n^e 
calcula n ((b,e) : t) = b*(n^e) + calcula n t 

--h
somaMonomio :: Monomio -> Polinomio -> Polinomio
somaMonomio m [] = [m]
somaMonomio (c,e) ((c',e') : t) 
            | e == e' = (c+c',e) : t 
            | otherwise = (c',e') : somaMonomio (c,e) t   

normaliza :: Polinomio -> Polinomio
normaliza [] = [] 
normaliza (h:t) = let tn = normaliza t 
                  in somaMonomio h tn
 
                  



