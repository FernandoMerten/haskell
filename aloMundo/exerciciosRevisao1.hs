msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort esquerda) (msort direita)
        where
            (esquerda, direita) = splitAt(div (length xs) 2) xs

            merge :: Ord a => [a] -> [a] -> [a]
            merge [] ys = ys
            merge xs [] = xs
            merge (x:xs) (y:ys)
                | x <= y = x : merge xs (y:ys)
                |otherwise = y : merge (x:xs) ys

--Ex2
mult :: Int -> Int -> Int -> Int
mult = \x  -> (\y -> (\z -> x * y * z))

--Ex3
halve :: [a] -> ([a], [a])
halve xs = (l1, l2)
    where
        (l1, l2) = splitAt (div (length xs) 2) xs

