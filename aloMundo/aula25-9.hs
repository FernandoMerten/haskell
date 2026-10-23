-- Um número perfeito é um número que a soma de todos seus dividendos resulta no próprio número
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum [divisor | divisor <- [1..(x-1)], x `mod` divisor == 0] == x]

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * (product xs) --não funcionou
