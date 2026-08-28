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

-- (Alternativamente, usando crases para o operador infixo: n = a div length xs, 
-- desde que length xs esteja devidamente isolado ou tratado).

-- 3. Primeira definição da função last
-- Uma forma comum de definir a função last (que retorna o último elemento de uma lista) 
-- utilizando funções já existentes na biblioteca padrão (head e reverse) é:


last :: [a] -> a
last xs = head (reverse xs)

-- 4. Outra definição para a função last
-- Podemos definir last de outras maneiras, seja por 
-- acesso direto via índice ou por recursão estrutural:

-- Utilizando índice e o tamanho da lista:

last1 :: [a] -> a
last1 xs = xs !! (length xs - 1)

-- Utilizando casamento de padrões e recursão:

last3 :: [a] -> a
last3 [x]    = x
last3 (_:xs) = last xs

-- 5. Duas formas diferentes de definir a função init
-- A função init remove o último elemento de uma lista. Ela pode ser definida de várias maneiras, destacando-se estas duas:

-- Primeira forma (invertendo a lista, removendo a cabeça com tail, e invertendo de volta):

init :: [a] -> [a]
init xs = reverse (tail (reverse xs))

-- Segunda forma (pegando todos os elementos exceto o último usando take e length):

init1 :: [a] -> [a]
init1 xs = take (length xs - 1) xs

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