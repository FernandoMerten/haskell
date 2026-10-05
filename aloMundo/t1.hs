--Bom dia sora, espero que o trabalho esteja perfeitamente alinhado com o solicitado

--1) Definir uma função recursiva que recebe um número binário (interpretado como número inteiro sem sinal) e
--retorna o valor equivalente em decimal. 𝑏𝑖𝑛2𝑑𝑒𝑐 ∷ [Bool] → 𝐼𝑛t
bin2dec :: [Bool] -> Int
bin2dec [] = 0
bin2dec (x:xs)
    | x == True    = (2 ^ length xs) + bin2dec xs
    | otherwise = bin2dec xs

--Exemplos: 
--Caso 1(Entrada Padrão)
--bin2dec [True, False, True] retorna 5
--Caso 2(Caso Limite)
--bin2dec [] retorna 0

--2) Definir uma função recursiva que recebe um número decimal inteiro não-negativo, um número de bits
--desejado e retorna o valor equivalente em binário (interpretado como número inteiro sem sinal) com o
--número de bits informado. Caso o número de bits não seja suficiente, escolha uma forma adequada de lidar
--com a situação, por exemplo, usando truncamento ou mensagem de erro. Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛 2 8 deve
--retornar [False,False,False,False,False,False,True,False]. 𝑑𝑒𝑐2𝑏𝑖𝑛 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 → [Bool]

dec2bin :: Int -> Int -> [Bool]
dec2bin n bits
    |bits <= 0 =[]
    |bits == 1 = [(n `mod` 2) == 1]
    |n >= 2^(bits-1) = [True]++ dec2bin (n-2^bits) (bits-1)
    |otherwise = [False]++ dec2bin n (bits-1)

--Exemplos: 
--Caso 1(Entrada padrão)
--dec2bin 2 8 retorna [False,False,False,False,False,False,True,False]
--Caso 2(Truncamento)
--dec2bin 5 2 retorna [True,True]
--Caso 3(Caso Limite)
--dec2bin _ 0 retorna []

--3) Definir uma função recursiva que recebe um número binário na representação de complemento de dois e
--retorna o valor equivalente em decimal inteiro. 𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙2𝑑𝑒𝑐 ∷ [Bool] → 𝐼𝑛t

bincompl2dec :: [Bool] -> Int
bincompl2dec [] = 0
bincompl2dec (x:xs)
    | x == True    = (-1 * (2 ^ length xs)) + bin2dec xs
    | otherwise = bin2dec xs

--Exemplos: 
--Caso 1(Entrada negativa)
--bincompl2dec [True,False,False,True] retorna -7
--Caso 2(Entrada Positiva)
--bincompl2dec [False,True,False,True,False] retorna 10
--Caso 3(Caso Limite)
--bincompl2dec [] retorna 0

--4) Definir uma função recursiva que recebe um número decimal inteiro, um número de bits desejado e retorna
--o valor equivalente em binário na representação de complemento de dois com o número de bits informado.
--Caso o número de bits não seja suficiente, escolha uma forma adequada de lidar com a situação, por
--exemplo, usando truncamento ou mensagem de erro. Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 (−2) 8 deve retornar
--[True,True,True,True,True,True,True,False]. 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 → [Bool]

dec2bincompl :: Int -> Int -> [Bool]
dec2bincompl n bits
    | bits <= 0 = []
    | bits == 1 = [(n `mod` 2) == 1]
    | otherwise = dec2bincompl (n `div` 2) (bits - 1) ++ [(n `mod` 2) == 1]

--Exemplos: 
--Caso 1(Entrada negativa)  !!!Necessário colocar parenteses se for usar número negativo na função sora!!!
--dec2bincompl (-2) 8 retorna [True,True,True,True,True,True,True,False]
--Caso 2(Truncamento)
--dec2bincompl (-2) 2 retorna [True,False]
--Caso 3(Caso Limite)
--dec2bincompl _ 0 retorna []

--5) Definir uma função recursiva que recebe dois números binários na representação de complemento de dois e
--um número de bits, retornando a soma binária destes valores considerando o limite de bits informado. A
--função deve assumir que: (i) os operandos possuem exatamente o número de bits indicado; (ii) o resultado
--também deve possuir exatamente esse número de bits. Além disso, a função deve indicar se ocorreu
--overflow durante a operação. A função deve retornar uma tupla no formato:
--(resultado, overflow), onde: (i) resultado é o número binário resultante da soma; (ii) overflow é um valor
--booleano que indica se houve estouro de capacidade. somarBin	::	[Bool]	->	[Bool]	->	Int	->	([Bool],Bool)

somarBin :: [Bool] -> [Bool] -> Int -> ([Bool], Bool)
somarBin [] _ _ =([],False)     --Nesses 2 primeiros casos eu poderia colocar que ele retorna o número binário sem somar nada, mas se esse fosse o 
somarBin _ [] _ =([],False)     --objetivo, então ao invés de uma lista vazia deveria se usar [False] que seria a representação de false.(Pelo menos na minha percepção)
somarBin (x:xs) (y:ys) bits
    |bits <= 0 = ([],False)
    |bits == 1 = ([x /= y], x && y)
    |otherwise = ((dec2bincompl ((bincompl2dec (x:xs)) + (bincompl2dec (y:ys))) bits), False)


--Exemplos
--Caso 1(Sem overflow)
--somarBin [False,True] [False,True] 3 retorna ([False,True,False],False)
--Caso 2(Com overflow)
--somarBin [True] [True] 1 retorna ([False],True)
--Caso 3(caso limite)
--somarBin [] [] 0 retorna ([], False)

-- PS: esse foi o que deu mais dor de cabeça com os parenteses no otherwise, mas eu que dei uma de burro mesmo
-- pois basta escrever certinho que é tranquilo, mas eu escrevi de qualquer jeito e fiquei tentando arrumar depois
-- levei cerca de meia hora pra entender como fazer. Depois começou a dar erro no caso de listas vazias pois eu tinha declarado 
-- "somarBin (x:xs) (y:ys) bits" direto, ai joguei o erro na IA e ela me disse que era por que quando ele chama (x:xs) ele ja está considerando 
-- que a lista possui pelo menos um elemento, então lembrei de quando vimos os "_" em aula, e essa foi uma parte em especifico que me chamou
-- bastante atenção, n sei o motivo, talvez por que to acostumado com java e o mais próximo disso que temos lá (pelo o que eu conheço) é o null
-- então achei uma mecânica bem diferente














































































-- Bom, se você veio até aqui, eu havia feito o trabalho anteirormente com tratando números binários como [Int] ao invés de [Bool] e eu gostei muito de
-- fazer esse trabalho e fiquei com pena de apagar, então resolvi apenas deixar ele aqui escondidinho com ' no fim da função pra não dar problema com as outras.




--1) Definir uma função recursiva que recebe um número binário (interpretado como número inteiro sem sinal) e
--retorna o valor equivalente em decimal. 𝑏𝑖𝑛2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛t
bin2dec' :: [Int] -> Int
bin2dec' [] = 0
bin2dec' (x:xs)
    | x == 1    = (2 ^ length xs) + bin2dec' xs
    | otherwise = bin2dec' xs

--Exemplos: 
--Caso 1(Entrada inválida)
--bin2dec [1, 3, 0, 1] retorna 9
--Caso 2(Caso Limite)
--bin2dec [] retorna 0

--2) Definir uma função recursiva que recebe um número decimal inteiro não-negativo, um número de bits
--desejado e retorna o valor equivalente em binário (interpretado como número inteiro sem sinal) com o
--número de bits informado. Caso o número de bits não seja suficiente, escolha uma forma adequada de lidar
--com a situação, por exemplo, usando truncamento ou mensagem de erro. Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛 2 8 deve
--retornar [0,0,0,0,0,0,1,0]. 𝑑𝑒𝑐2𝑏𝑖𝑛 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 → [𝐼𝑛𝑡]

dec2bin' :: Int -> Int -> [Int]
dec2bin' n bits
    |bits <= 0 =[]
    |bits == 1 = [n `mod` 2]
    |n >= 2^(bits-1) = [1]++ dec2bin' (n-2^bits) (bits-1)
    |otherwise = [0]++ dec2bin' n (bits-1)

--Exemplos: 
--Caso 1(Entrada padrão)
--dec2bin' 2 8 retorna [0,0,0,0,0,0,1,0]
--Caso 2(Truncamento)
--dec2bin' 5 2 retorna [1,1]
--Caso 3(Caso Limite)
--bin2dec [] retorna 0

--3) Definir uma função recursiva que recebe um número binário na representação de complemento de dois e
--retorna o valor equivalente em decimal inteiro. 𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛t

bincompl2dec' :: [Int] -> Int
bincompl2dec' [] = 0
bincompl2dec' (x:xs)
    | x == 1    = (-1 * (2 ^ length xs)) + bin2dec' xs
    | otherwise = bin2dec' xs

--Exemplos: 
--Caso 1(Entrada negativa)
--bincompl2dec' [1,0,0,1] retorna -7
--Caso 2(Entrada Positiva)
--bincompl2dec' [0,1,0,1,0] retorna 10
--Caso 3(Caso Limite)
--bincompl2dec' [] retorna 0
--PS: Não sei se era a solução que tu esperava sora, mas imagino que não tenha problema eu usar outra função de outra questão do mesmo trabalho como função auxiliar
-- eu não sei se conseguiria pensar em uma forma de resolver isso sem utilizar funções auxiliares ou adcionar um parametro a mais na função original

--4) Definir uma função recursiva que recebe um número decimal inteiro, um número de bits desejado e retorna
--o valor equivalente em binário na representação de complemento de dois com o número de bits informado.
--Caso o número de bits não seja suficiente, escolha uma forma adequada de lidar com a situação, por
--exemplo, usando truncamento ou mensagem de erro. Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 (−2) 8 deve retornar
--[1,1,1,1,1,1,1,0]. 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 → [𝐼𝑛𝑡]

dec2bincompl' :: Int -> Int -> [Int]
dec2bincompl' n bits
    | bits <= 0 = []
    | bits == 1 = [n `mod` 2]
    | otherwise = dec2bincompl' (n `div` 2) (bits - 1) ++ [n `mod` 2]

--Exemplos: 
--Caso 1(Entrada negativa)
--dec2bincompl' (-2) 8 retorna [1,1,1,1,1,1,1,0]         !!!Necessário colocar parenteses se for usar número negativo na função sora!!!
--Caso 2(Truncamento)
--dec2bincompl' (-2) 2 retorna [1,0]
--Caso 3(Caso Limite)
--dec2bincompl' [] retorna 0

--5) Definir uma função recursiva que recebe dois números binários na representação de complemento de dois e
--um número de bits, retornando a soma binária destes valores considerando o limite de bits informado. A
--função deve assumir que: (i) os operandos possuem exatamente o número de bits indicado; (ii) o resultado
--também deve possuir exatamente esse número de bits. Além disso, a função deve indicar se ocorreu
--overflow durante a operação. A função deve retornar uma tupla no formato:
--(resultado, overflow), onde: (i) resultado é o número binário resultante da soma; (ii) overflow é um valor
--booleano que indica se houve estouro de capacidade. somarBin'	::	[Int]	->	[Int]	->	Int	->	([Int],	Bool)

somarBin' :: [Int] -> [Int] -> Int -> ([Int], Bool)
somarBin' [] _ _ =([],False)
somarBin' _ [] _ =([],False)
somarBin' (x:xs) (y:ys) bits
    |bits <= 0 = ([],False)
    |bits == 1 = ([(x+y) `mod` 2],True)
    |otherwise = ((dec2bincompl' ((bincompl2dec' (x:xs)) + (bincompl2dec' (y:ys))) bits), False)


--Exemplos
--Caso 1(Sem overflow)
--somarBin' [0,1] [0,1] 3 retorna ([0,1,0],False)
--Caso 2(Com overflow)
--somarBin' [1] [1] 1 retorna ([0],True)
--Caso 3(caso limite)
--somarBin' [] [] 0 retorna ([], False)

--PS: esse foi o que deu mais dor de cabeça com os parenteses no otherwise, mas eu que dei uma de burro mesmo
-- pois basta escrever certinho que é tranquilo, mas eu escrevi de qualquer jeito e fiquei tentando arrumar depois
-- levei cerca de meia hora pra entender como fazer. Depois começou a dar erro no caso de listas vazias pois eu tinha declarado 
-- "somarBin' (x:xs) (y:ys) bits" direto, ai joguei o erro na IA e ela me disse que era por que quando ele chama (x:xs) ele ja está considerando 
-- que a lista possui pelo menos um elemento, então lembrei de quando vimos os "_" em aula, e essa foi uma parte em especifico que me chamou
-- bastante atenção, n sei o motivo, talvez por que to acostumado com java e o mais próximo disso que temos lá (pelo o que eu conheço) é o null
-- então achei uma mecânica bem diferente