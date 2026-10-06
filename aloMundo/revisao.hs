-- Ex1:
-- Definição da função principal msort
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort metade1) (msort metade2)
  where
    -- Divide a lista exatamente no meio
    (metade1, metade2) = splitAt (length xs `div` 2) xs

-- Função auxiliar para combinar (intercalar) duas listas já ordenadas
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys                        -- Se a primeira lista acabar, retorna a segunda
merge xs [] = xs                        -- Se a segunda lista acabar, retorna a primeira
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)     -- se x <= y então pega o primeiro elemento da lista x e chama recursivamente o tail de x e a lista y
  | otherwise = y : merge (x:xs) ys     -- Caso contrário faz exatamente a mesma coisa só que trocando x e y

-- Notas da questão:
-- "A" : "BC" -> "ABC"
-- splitAt retorna uma tupla de 2 listas e recebe por parametro onde quer que seja dividia a lista e a lista

-- Ex2:
mult :: Int -> Int -> Int -> Int
-- mult x y z = x * y * z
mult = \x -> (\y -> (\z -> x * y * z))
-- mult = \x y z -> x*y*z     --(metodo abreviado)

-- Notas da questão:
-- \ é usada para pegar um dos parametros e chamar outra função com os outros parametros

-- Ex3:
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = splitAt (length xs `div` 2) xs

-- Notas da questão:
-- Acho que só era pra eu aprender splitAt nessa questão

-- Ex4:
third ::Num a => [a] -> a
third [] = 0
third [_] = 0
third [_,_] = 0
third xs = xs !! 2
--third xs = head(tail(tail xs))
--third (_:_:x:_) = x

-- Ex5
matriz :: Int -> Int -> [(Int,Int)]
matriz x y = if (x<0 || y<0) then [] else reverse([(x,y) | x <- [x, (x-1) .. 0], y <- [y,(y-1) .. 0]])


-- Ex6
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ (replicate' (n-1) x)

replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' n x = [x | _ <- [1 .. n]]


-- Ex7
multiplic :: Int -> Int -> Int
multiplic 0 _ = 0
multiplic _ 0 = 0
multiplic n x = x+multiplic (n-1) x


-- Ex8
euclid :: Int -> Int -> Int
euclid a b
    | a >= b = if ((mod a b) /= 0) then euclid b (mod a b) else b
    |otherwise = if ((mod b a) /= 0) then euclid a (mod b a) else a

-- Notas da questão:
-- /= é diferente
-- mod x y pega o resto da divisão x/y
-- o algoritmo de euclides é confuso