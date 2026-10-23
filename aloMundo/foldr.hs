--Arquivo de estudo

--A função foldl recebe uma função e uma lista, e retorna uma lista
--ela aplica a função a todos elementos da lista, existe uma função semelhante a ela chamada foldl
--foldr faz o caminho inverso de foldl, começando a iteração pelo ultimo elemento da lista e considerando
--ele o primeiro elemento a entrar na função, diferente de foldl

--foldl (/) 64 [4,2,4]
--dessa forma divide 64 por 4
--depois pega o resultado (16) e divide por 2
--depois pega o resultado (8) e divide por 4 novamente, resultando em 2, e retorna 2

--Exercicios:

--1. Aquecimento: Multiplicação de Elementos (produto)
--Escreva uma função que calcula o produto (multiplicação) de todos os números de uma lista. Se a lista estiver vazia, o resultado deve ser 1.
produto :: Num a => [a] -> a
produto xs = foldr (*) 1 xs
--pode tambem omitir
--produto = foldr (*) 1



--2. Intermediário: Filtrando Elementos (filtrar)
--Recrie o comportamento da função nativa filter. Sua função deve receber um predicado (uma função que retorna um Booleano) e uma lista, retornando uma nova lista contendo apenas os elementos que atendem ao predicado.

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar condicao lista = foldr (\x ac -> if condicao x then x : ac else ac) [] lista
-- \x ac declara uma função com dois parametros, x e ac
-- if condicao x aplica a função determinada ao elemento atual pra fazer o teste condicional
-- then x : ac se verdadeiro coloca x (elemento atual) no inicio de ac (lista acumuladora)
-- else ac diz pro foldr pegar o próximo elemento e manter ac do jeito que está
-- na primeira iteração, ac = []


--3. Desafio: Contando Elementos (comprimento)
--Escreva uma função que calcula o tamanho (número de elementos) de uma lista usando foldr. Dica: você precisará ignorar o valor do elemento atual no acumulador.

comprimento :: [a] -> Int
--comprimento lista = foldr (\x ac -> ac+1) 0 lista
--mais elegante
comprimento lista = foldr (\_ ac -> ac+1) 0 lista

--4. Avançado: Invertendo uma Lista (reverter)
--Implemente uma função que inverte a ordem dos elementos de uma lista usando foldr. Dica: lembre-se de como o foldr processa os elementos da direita para a esquerda e como você pode juntar listas.
--Exemplo: reverter [1, 2, 3] deve retornar [3, 2, 1]
reverter :: [a] -> [a]
reverter lista = foldr (\x ac -> ac++[x]) [] lista

--Opção com foldl melhor otimizada na questão de desempenho
reverterOtimizado :: [a] -> [a]
reverterOtimizado lista = foldl (\ac x -> x : ac) [] lista
