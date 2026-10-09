somar :: Int -> Int -> Int
somar x y = x+y

somav2 :: Int -> (Int->Int)
somav2 = \x -> (\y -> x+y)

twice2 :: (a -> a) -> a -> a
twice2 f x  = f (f x)


-- map (a -> a) -> [a] -> [a]
-- map f (x:xs) = f x : map f xs

somatorio :: Num a => [a] -> a
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs
-- somatorio [1,2,3]

--
-- somatorio 1 + somatorio [2,3] 
-- somatorio 2 + somatorio [3]
-- somatorio 3 + somatorio []

-- somatoriofoldr :: Num a => [a] -> [a]
