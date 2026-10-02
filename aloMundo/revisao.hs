-- head retorna só o primeiro elemento/take retorna os primeiros elementos
-- ++ concatena listas
msort :: Ord a => [a] -> [a]
msort xs
    | length(xs) <= 1 = xs
    |otherwise = (take(length((xs)) `div` 2) xs)
  --  | otherwise = (take(length(xs) `div` 2) xs)
  --  where 
     --   ordenar :: [a] -> [a]
   --     ordenar ys = if x <= y then xs++ys else ys ++ ys++xs



-- | otherwise [msort(take(length((xs)) `div` 2)) ++  msort(tail(length((xs))`div` 2))]
-- ys : take(length((xs)) ´div´ 2) xs : tail(length((xs))´div´ 2)