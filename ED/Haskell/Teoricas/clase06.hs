
-- Propósito:
-- Devuelve el mínimo elemento de la lista de números dada. 
-- Pre:
-- La lista de números debe tener al menos un elemento.
-- Complejidad: O(n), siendo n la cantidad de elementos de la
-- lista. 
--
minimo :: [Int] -> Int 
minimo xs = minimo' xs (head xs) 

minimo' :: [Int] -> Int -> Int
minimo' [] m     = m 
minimo' (x : xs) m 
    | x < m = minimo' xs x
    | otherwise = minimo' xs m 

-- Propósito:
-- Indica si en la lista dada hay elementos repetidos. 
-- Complejidad: O(n²). 
-- 
hayRepetidos :: Eq a => [a] -> Bool
hayRepetidos [] = False
hayRepetidos (x : xs) = elem x xs || hayRepetidos xs

-- Propósito:
-- Devuelve el elemento en la posición milésima de la lista dada. 
-- Pre: Debe haber al menos 1000 elementos en la lista. 
-- Complejidad: O(1).
--
milesimoElemento :: [a] -> a 
milesimoElemento xs = milesimoElemento' xs 1

milesimoElemento' :: [a] -> Int -> a
milesimoElemento' (x : xs) contador = 
    if contador == 1000
        then x 
        else milesimoElemento' xs (contador + 1)

-- Propósito:
-- Devuelve el mínimo de los tres números dados. 
-- Complejidad: O(1). 
--
min3 :: Int -> Int -> Int -> Int 
min3 x y = min (min x y)  

-- Propósito:
-- Devuelve la longitud de la lista de elementos dada. 
-- Complejidad: O(n). 
--
longitud :: [a] -> Int 
longitud []       = 0 
longitud (x : xs) = 1 + longitud xs 

