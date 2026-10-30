--tipos de classes
type Pos = (Int, Int)
origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

right :: Pos -> Pos
right (x,y) = (x+1,y)

down :: Pos -> Pos
down (x,y) = (x,y-1)

up :: Pos -> Pos
up (x,y) = (x,y+1)

type Pair a = (a,a)

copy :: a -> Pair a
copy x = (x,x)


--Exercicio:
--considere um tipo de dados que representa movimentos
--(dica use o pos) em um plano: north, south, east, west.
--Implemente uma função que recebe um movimento em uma Pos(x,y)
--e retorne a nova posição após o movimento

--definição do tipo de dados
data Move = North | South | East | West deriving (Show)
 
 --definição da função que usa o tipo Move
-- Move faz parte da classe Show Isto É: tudo é convertido para string
-- e assim, essas strings podem ser vistas no console

mover :: Move -> Pos -> Pos
mover North (x,y) = (x,y+1)
mover South (x,y) = (x,y-1)
mover East (x,y) = (x+1,y)
mover West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (mover m p)

--moves [South, East, East] origin



data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)
--let arv = Node (Leaf 2) 1 (Leaf 3)

--faça uma função que conte a ocorrência de um valor nessa árvore
-- ocurrs 3 arv -> True
-- ocurrs 4 arv -> False
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
