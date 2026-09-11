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

--copilot estou com fome, o que me recomenda?Se você está com fome, aqui estão algumas recomendações dependendo do que você gosta:
--1. **Comida rápida**: Se você quer algo rápido e fácil, pode pedir um hambúrguer, pizza ou sanduíche.
--2. **Comida saudável**: Se você prefere algo mais leve, uma salada, um wrap de frango ou um smoothie podem ser boas opções.
--3. **Comida caseira**: Se você gosta de cozinhar, pode preparar um prato simples como macarrão, arroz com legumes, ou uma omelete.
--4. **Lanches**: Se você quer apenas um lanche, frutas, iogurte, barras de cereal ou um sanduíche pequeno podem ser ótimas escolhas.
--5. **Doces**: Se você está com vontade de algo doce, pode optar por chocolate, sorvete, bolo ou biscoitos.
--e se eu não tenho dinheiro? Se você não tem dinheiro, aqui estão algumas opções que podem ajudar:
--1. **Cozinhar em casa**: Use ingredientes que você já tem em casa para preparar uma refeição simples. Arroz, feijão, ovos, macarrão e legumes são ótimas opções que geralmente estão disponíveis.
--2. **Aproveitar sobras**: Se você tem sobras de refeições anteriores, tente reutilizá-las para criar um novo prato.
--3. **Troca ou doação**: Verifique se há programas de troca de alimentos ou doações em sua comunidade. Muitas vezes, organizações locais oferecem alimentos gratuitos ou a preços reduzidos.
--4. **Plantas comestíveis**: Se você tem acesso a um jardim ou área verde, considere cultivar algumas plantas comestíveis, como ervas, tomates ou alface. Isso pode fornecer alimentos frescos sem custo adicional.
--5. **Reduzir desperdício**: Planeje suas refeições para evitar desperdício e aproveite ao máximo os alimentos que você já possui.