-- exercicio 1
maxMult5 :: [Int] -> Int
maxMult5 lista = maximum [x | x <- lista, x `mod` 5 == 0]

-- exercicio 2
nElemento :: Int -> [a] -> a
nElemento n lista = lista !! n

-- exercicio 3
freqUm :: Eq a => a -> [a] -> Bool
freqUm elem lista = length (filter (==elem) lista) == 1

--exercicio 4
maiores_que :: Int -> [Int] -> [Int]
maiores_que _ [] = []
maiores_que x (y:ys)
  | y > x     = y : maiores_que x ys
  | otherwise = maiores_que x ys

--exercicio 5
remUm :: Eq a => a -> [a] -> [a]
remUm _ [] = []
remUm x (y:ys)
  | x == y    = ys
  | otherwise = y : remUm x ys

--exercicio 6
interseccao :: [Int] -> [Int] -> [Int]
interseccao [] _ = []
interseccao _ [] = []
interseccao (x:xs) ys
  | pertence x ys = x : interseccao xs ys
  | otherwise     = interseccao xs ys

pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence y (z:zs)
  | y == z    = True
  | otherwise = pertence y zs

--exercicio 7
sequencia :: Integer -> Integer -> [Integer]
sequencia 0 _ = []
sequencia n m = [m..m+n-1]

--exercicio 8
ordena :: [Int] -> [Int]
ordena lista = bubbleSort lista

bubbleSort :: [Int] -> [Int]
bubbleSort [] = []
bubbleSort lista = bubbleSort' lista (length lista)

bubbleSort' :: [Int] -> Int -> [Int]
bubbleSort' lista 0 = lista
bubbleSort' lista n = bubbleSort' (trocarMenor lista) (n - 1)

trocarMenor :: [Int] -> [Int]
trocarMenor [x] = [x]
trocarMenor (x:y:zs)
  | x > y     = y : trocarMenor (x:zs)
  | otherwise = x : trocarMenor (y:zs)

--exercicio 9
ordenaIns :: [Int] -> Int -> [Int]
ordenaIns lista n = insereOrdenado (ordena lista) n

insereOrdenado :: [Int] -> Int -> [Int]
insereOrdenado [] n = [n]
insereOrdenado (x:xs) n
  | n <= x    = n : x : xs
  | otherwise = x : insereOrdenado xs n

--exercicio 10
ehOrdenado :: [Int] -> Bool
ehOrdenado [] = True
ehOrdenado [_] = True
ehOrdenado (x:y:xs)
  | x <= y    = ehOrdenado (y:xs)
  | otherwise = False