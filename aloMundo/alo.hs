add :: (Int, Int) -> Int
add (x, y) = x + y

add2 :: (Int, Int) -> Int
add2 (x,y) = sum[x,y]

-- 1. Execução no GHCi
-- Para realizar esta etapa, abra o terminal, digite ghci para iniciar 
-- o interpretador do Haskell e teste as expressões e funções básicas 
-- abordadas nos slides iniciais do curso (como operações aritméticas, 
-- manipulação de listas com head, tail, take, drop, length, entre outras).

-- 2. Correção de erros de sintaxe
-- O programa original apresenta três problemas: o uso de aspas gráficas/curvas 
-- incorretas no operador div, a ausência de parênteses na aplicação de função/operador, e 
-- a indentação do bloco where. O código corrigido é:

n :: Int
n = div a (length xs)
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

----------------------------------------------------------------------------------------------------------

-- add :: (Int, Int) -> Int -- definição de uma função 
                         -- nome: add (minuscula)
                         -- tipo de entrada: (x,y), onde sao inteiros
                         -- retorno : z -- inteiro
add3 :: (Int, Int) -> Int
add3 (x, y) = x + y
-- > add (2,4)

--Exec 3
last2 ::  [Int] -> Int
last2 xs = head (reverse xs)

-------------------------------------------------------------------------------------------------
--Exec 4
-- Utilizando índice e o tamanho da lista:
last5 :: [a] -> a
last5 xs = xs !! (length xs - 1)

-- Utilizando casamento de padrões e recursão:

last6 :: [a] -> a
last6 [x]    = x
last6 (_:xs) = last6 xs

-------------------------------------------------------------------------------------------------
--Exercício 5
--Defina a função init2 que recebe uma lista e retorna a lista sem o último elemento
init2 :: [Int] -> [Int]
-- tail -> retorna todos os elementos de uma lista, exceto o primeiro
init2 xs = reverse(tail(reverse xs))

init3:: [Int] -> [Int]
init3 xs = take (length xs - 1) xs
l = [1, 2, 3, 4, 5]