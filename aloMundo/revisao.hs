-- Definição da função principal msort
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort metade1) (msort metade2)
  where
    -- Divide a lista exatamente no meio
    (metade1, metade2) = splitAt (length xs `div` 2) xs

-- Função auxiliar para combinar (intercalar) duas listas já ordenadas
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys                        -- Se a primeira lista acabar, retorna a segunda
merge xs [] = xs                        -- Se a segunda lista acabar, retorna a primeira
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)     -- Se o elemento da esquerda for menor, ele vem primeiro
  | otherwise = y : merge (x:xs) ys     -- Caso contrário, o da direita vem primeiro
