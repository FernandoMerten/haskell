-- Implemente em Haskell uma função chamada pair que receba dois valores e
-- retorne uma tupla contendo esses dois valores, mantendo a mesma ordem em
-- que foram recebidos.

pair :: Int -> Int -> [Int]
pair x y = [x, y]

-- Implemente em Haskell uma função que receba uma lista e retorne o segundo
-- elemento dessa lista.
-- Considere que a lista recebida sempre possuirá pelo menos dois elementos.

-- !! serve para acessar um elemento de uma lista em uma posição específica, 
-- onde a contagem começa do zero.
segundo :: [Int] -> Int
segundo x =  head (tail x)

-- Implemente em Haskell uma função chamada `palindromo` que receba uma lista
-- e produza uma nova lista formada pela lista original seguida dos seus elementos
-- em ordem inversa.

-- ++ serve para concatenar duas listas em Haskell.
palindromo :: [a] -> [a]
palindromo x = x ++ reverse x
palavra = "Socorram-me, subi no onibus em Marrocos."