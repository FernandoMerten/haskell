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
--Exec 4 -> ultimo elemento de uma lista
-- Utilizando índice e o tamanho da lista:
last5 :: [Int] -> Int
last5 xs = xs !! (length xs - 1)

-- Utilizando casamento de padrões e recursão:

last6 :: [Int] -> Int
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

------------------------------------------------------------------------------------------------------

-- Booleanos
-- Em haskell booleanos são representados pelo tipo Bool, 
-- que possui dois valores possíveis: True e False.
-- Exemplo de função que verifica se uma lista está vazia:
-- isEmpty :: [Int] -> Bool
-- isEmpty xs = length xs == 0

-- e::t -> Se a avaliação de uma expressão e produziria
-- uma valor do tipo t, então dizemos que e é do
-- tipo t, anotado
-- Exemplo:
-- 5 :: Int
-- 5 é do tipo Int, anotado como 5 :: Int

-- :Type -> comando do GHCi para verificar o tipo de uma expressão
-- Exemplo de uso no GHCi:
-- > :type not False
-- not False :: Bool

--TiPOS basicos: 
-- Int    -> inteiros
-- Bool   -> booleanos (True ou False)
-- Char   -> caracteres
-- String -> sequências de caracteres
-- [Int]  -> listas de inteiros
-- [Bool] -> listas de booleanos
-- [Char] -> listas de caracteres (equivalente a String)

--Tuplas -> agrupamentos de valores de diferentes tipos
-- Exemplo:
-- (Int, Bool) -> uma tupla contendo um inteiro e um booleano
-- (3, True) :: (Int, Bool)
-- (Int, Char, Bool) -> uma tupla contendo um inteiro, um caractere e um booleano
-- (5, 'a', False) :: (Int, Char, Bool)
-- (Int, Int, Int) -> uma tupla contendo três inteiros
-- (1, 2, 3) :: (Int, Int, Int)
-- Tupla com uma lista dentro:
-- ([Int], Bool) -> uma tupla contendo uma lista de inteiros e um booleano
-- ([1, 2, 3], True) :: ([Int], Bool)

-- Em Haskell, uma String é uma lista de caracteres ([Char])
-- Portanto, você pode usar a função length para contar o número de caracteres em uma String
-- Exemplo:
-- length "Hello" -> 5
-- length ""      -> 0
-- Para percorrer uma String, você pode usar funções de listas, como head, tail, take, drop, etc.
-- Exemplo:
-- head "Hello" -> 'H'
-- tail "Hello" -> "ello"
-- take 3 "Hello" -> "Hel"
-- drop 2 "Hello" -> "llo"
-- reverse "Hello" -> "olleH"

-- Função é um bloco de código que recebe argumentos e retorna um valor.
-- Exemplo de função que soma dois números:
-- soma :: Int -> Int -> Int
-- soma x y = x + y
-- usamos a seta -> para indicar o tipo de entrada e saída da função.
-- Curried functions: Em Haskell, todas as funções são curried por padrão. 
-- Isso significa que uma função que recebe múltiplos argumentos pode ser chamada com menos argumentos, 
-- retornando uma nova função que espera os argumentos restantes.
-- Exemplo de função curried:
-- somaCurried :: Int -> Int -> Int
-- somaCurried x y = x + y
-- Você pode chamar a função com menos argumentos:
-- let somaCom2 = somaCurried 2
-- somaCom2 3 -> 5
-- usamos Int -> Int -> Int, porque a função recebe um Int e retorna uma função que recebe outro Int e retorna um Int. 
-- usamos curried para permitir a aplicação parcial de funções, o que é útil em programação funcional.

-- Associatividade de operadores: Em Haskell, a maioria dos operadores é associativa à esquerda.
-- Isso significa que, quando você tem uma expressão com múltiplos operadores do mesmo tipo,
-- eles são avaliados da esquerda para a direita.
-- Exemplo:
-- 1 + 2 + 3 é avaliado como (1 + 2) + 3 -> 6
-- 4 - 2 - 1 é avaliado como (4 - 2) - 1 -> 1
-- 2 * 3 * 4 é avaliado como (2 * 3) * 4 -> 24
-- 8 `div` 2 `div` 2 é avaliado como (8 `div` 2) `div` 2 -> 2

----------------------------------------------------------------------------------------------------
quadrado :: Num a => a ->a
quadrado x = x * x

-- usamos esse termo => para indicar que a função quadrado é polimórfica, ou seja, 
-- ela pode receber qualquer tipo numérico (Int, Float, Double, etc.) e retornar o mesmo tipo.
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

