factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x+y*y == z*z]
 
--como funciona a função count:  | compara
count :: Char -> String -> Int
count c xs = length [c | x <- xs, x == c]
 
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']