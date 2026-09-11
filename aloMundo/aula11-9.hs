--com if-then-else
safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

--com guardas
safetailB :: [a] -> [a]
safetailB xs
            | null xs = []
            | otherwise = tail xs

--Casamento de padrões 
safetailC :: [a] -> [a]
safetailC [] = []
safetailC (_:xs) = xs

--Exercicio 2
or' False False = False
or' False True = True
or' True False = True   
or' True True = True

or'' False False = False
or'' _ _ = True

or''' False b = b
or''' True _ = True

or'''' a b | a == b = a
          | otherwise = True

and' :: Bool -> Bool -> Bool
and' a b = if a && b then True else False

and'' :: Bool -> Bool -> Bool
and'' a b = if a then b else False