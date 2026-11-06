data Expre = Val Int | Add Expre Expre | Mult Expre Expre deriving (Show,Eq)

sizze :: Expre -> Int
sizze (Val n) = 1
sizze (Add x y) = sizze x + sizze y 
sizze (Mult x y) = sizze x + sizze y



eval :: Expre -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y 
eval (Mult x y) = eval x * eval y

--let e = Add (Val 1) (Mult (Val 2)(Val 3))


--Functores

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

incv2 :: [Int] -> [Int]
incv2 = map (+1)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)
instance Functor Tree where