module Clase06 where

-- Propósito:
-- Devuelve el mínimo elemento de la lista de ordenables dada. 
-- Pre: La lista de ordenables debe tener al menos un elemento.
-- Complejidad: O(n), siendo n la longitud de la lista (lineal). 
--
minimo :: Ord a => [a] -> a
minimo xs = minimo' xs (head xs)

minimo' :: Ord a => [a] -> a -> a
minimo' [] m     = m
minimo' (x : xs) m
    | x < m     = minimo' xs x
    | otherwise = minimo' xs m

-- Propósito:
-- Indica si en la lista dada hay elementos repetidos. 
-- Complejidad: O(n²) (cuadrático). 
-- 
hayRepetidos :: Eq a => [a] -> Bool
hayRepetidos []       = False
hayRepetidos (x : xs) = elem x xs || hayRepetidos xs

-- Propósito:
-- Devuelve el elemento en la posición milésima de la lista dada. 
-- Pre: Debe haber al menos 1000 elementos en la lista. 
-- Complejidad: O(1) (constante).
--
milesimoElemento :: [a] -> a
milesimoElemento xs = milesimoElemento' xs 1

milesimoElemento' :: [a] -> Int -> a
milesimoElemento' xs contador =
    if contador == 1000
        then head xs
        else milesimoElemento' (tail xs) (contador + 1)

-- Propósito:
-- Devuelve el mínimo de los tres números dados. 
-- Complejidad: O(1). 
--
min3 :: Int -> Int -> Int -> Int
min3 x y = min (min x y) -- Reducción eta

-- Propósito:
-- Devuelve la longitud de la lista de elementos dada. 
-- Complejidad: O(n). 
--
longitud :: [a] -> Int
longitud []       = 0
longitud (x : xs) = 1 + longitud xs

-- Propósito:
-- Concatena dos listas dadas en una sola.
-- Complejidad: O(n), siendo n la longitud de la primera lista.
--
append :: [a] -> [a] -> [a]
append [] ys       = ys
append (x : xs) ys = x : append xs ys

-- Propósito:
-- Agrega un elemento al final de la lista dada.
-- Complejidad: O(n).
--
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal xs e = foldr (:) [e] xs

-- Propósito:
-- Devuelve el elemento en la i-ésima posición de la lista (índice 0).
-- Pre: El índice i debe ser mayor o igual a 0 y menor a la longitud de la lista.
-- Complejidad: O(n) (en el peor caso recorre hasta la posición i).
--
iesimo :: Int -> [a] -> a
iesimo i xs = iesimo' i xs 0

iesimo' :: Int -> [a] -> Int -> a
iesimo' i xs contador =
    if contador == i
        then head xs
        else iesimo' i (tail xs) (contador + 1)

-- Propósito:
-- Indica si un elemento pertenece a la lista dada.
-- Complejidad: O(n), siendo n la cantidad de comparaciones de elementos.
--
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x : xs) =
    e == x || pertenece e xs

-- Propósito:
-- Devuelve una lista con los elementos comunes entre las dos listas dadas.
-- Complejidad: O(n * m), siendo n la longitud de la primera lista y m la de la segunda.
--
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] _  = []
interseccion xs ys =
    if pertenece (head xs) ys
        then head xs : interseccion (tail xs) ys
        else interseccion (tail xs) ys

-- Propósito:
-- Invierte el orden de los elementos de la lista dada.
-- Complejidad: O(n²).
--
reverse' :: [a] -> [a]
reverse' []       = []
reverse' (x : xs) =
      reverse' xs ++ [x]

-- Propósito:
-- Devuelve la lista sin su primer elemento.
-- Pre: La lista no debe ser vacía.
-- Complejidad: O(1).
-- 
tail' :: [a] -> [a]
tail' (x : xs) = xs

-- Propósito:
-- Devuelve una lista sin elementos repetidos, conservando la primera aparición.
-- Complejidad: O(n²).
--
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []       = []
sinRepetidos (x : xs) =
     if pertenece x xs
        then sinRepetidos xs
        else x : sinRepetidos xs

-- Propósito:
-- Ordena la lista de menor a mayor (Selection Sort).
-- T(n) = T(n-1) + O(n)
-- Complejidad: O(n²)
--
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs =
    m : ordenar (removerElemento xs m)
        where m = minimo xs

-- Propósito:
-- Elimina la primera aparición del elemento dado en la lista.
-- Complejidad: O(n).
removerElemento :: Ord a => [a] -> a -> [a]
removerElemento (x : xs) e =
    if x == e
        then xs
        else x : removerElemento xs e