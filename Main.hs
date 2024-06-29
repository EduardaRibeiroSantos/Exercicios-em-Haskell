fatorialDuplo :: Integer -> Integer
fatorialDuplo n
  | n == 0 = 1
  | n == 1 = 1
  | n > 1 = fatorialDuplo (n - 2) * n

multIntervalo :: Integer -> Integer -> Integer
multIntervalo m n
  | m > n = 1
  | m > 0 && n > 0 = multIntervalo (m + 1) (n - 1) * n * m

newSoma :: Integer -> Integer -> Integer
newSoma m n
  | n == 0 = m
  | n > 0 = newSoma (succ (m)) (pred (n))

raizQuadradaInteira :: Integer -> Integer
raizQuadradaInteira n = quadrado n (pred n)

quadrado::Integer -> Integer -> Integer
quadrado n m
  |n >= m * m = m
  |otherwise = quadrado n (pred m)

resto::Integer -> Integer -> Integer
resto m n
  |m == n = 0
  |m == 0 = 0
  |m == 1 = 1
  |m >= n = resto (m - n) n
  |otherwise = m
  
quociente::Integer -> Integer -> Integer
quociente m n
  |m == n = 1
  |m > n = succ (quociente (m - n) n)
  |otherwise = 0

potencia::Integer -> Integer -> Integer
potencia m n
  |n == 0 = 1
  |n > 0 = m * potencia m (n - 1)

minEmax :: Integer -> Integer -> Integer -> (Integer, Integer)
minEmax x y z = ((menor x y z), (maior x y z))

maior :: Integer -> Integer -> Integer -> Integer
maior x y z
  |x >= y && x >= z = x
  |y >= x && y >= z = y
  |z >= x && z >= y = z

menor :: Integer -> Integer -> Integer -> Integer
menor x y z
  |x <= y && x <= z = x
  |y <= x && y <= z = y
  |z <= x && z <= y = z 

removeLin1 :: [(Integer,Integer)] -> [(Integer,Integer)]
removeLin1 [] = []
removeLin1 lista
  |fst(head lista) == 1 = removeLin1 (tail lista)
  |otherwise = head lista : removeLin1 (tail lista)

trd :: (Integer,Integer,Integer) -> Integer
trd (_,_,z) = z

func :: [(Integer,Integer,Integer)] -> Integer
func [] = 0
func lista = trd (head lista) + func (tail lista)

numera :: [String] -> [(Integer,String)]
numera [] = []
numera lista = contabiliza lista 1

contabiliza :: [String] -> Integer -> [(Integer,String)]
contabiliza lista x
  |lista == [] = []
  |otherwise = (x, head lista) : contabiliza (tail lista) (x + 1)

ex10 lista = map length lista

getList func lista = filter func lista

sucessor = (\x -> x + 1)

duasVezes f x = f (f x)  

mapear1 :: (a -> b) -> [a] -> [b]
mapear1 f [] = []
mapear1 f (x:xs) = f x : mapear1 f xs
-- mapear1 sucessor [1,2,3]
-- mapear1 (duasVezes sucessor) [1,2,3]
mapear2 f lista = [(f x ) | x <- lista] 

mapear3 f lista = map f lista