module Conjunto (Conjunto, vacioC, agregarC, perteneceC, cantidadC, borrarC, unionC, listaC) where

---- Interfaz del módulo Conjunto
--

-- Crea un conjunto vacío.
--
vacioC :: Conjunto a

-- Propósito:
-- Dados un elemento y un conjunto, agrega el elemento al 
-- conjunto.
--
agregarC :: Eq a => a -> Conjunto a -> Conjunto a

-- Propósito:
-- Dados un elemento y un conjunto indica si el elemento 
-- pertenece al conjunto.
--
perteneceC :: Eq a => a -> Conjunto a -> Bool

-- Propósito:
-- Devuelve la cantidad de elementos distintos de un conjunto.
--
cantidadC :: Eq a => Conjunto a -> Int

-- Propósito:
-- Borra del conjunto dado el elemento dado. Si el elemento 
-- no pertenece al conjunto, deja al conjunto igual.
--
borrarC :: Eq a => a -> Conjunto a -> Conjunto a


-- Propósito:
-- Dados dos conjuntos devuelve un conjunto con todos los elementos
-- de ambos conjuntos.
--
unionC :: Eq a -> Conjunto a -> Conjunto a -> Conjunto a

-- Propósito:
-- Dado un conjunto devuelve una lista con todos los elementos
-- distintos del conjunto.
listaC :: Eq a => Conjunto a -> [a]

---- Implementación del módulo Conjunto
--

data Conjunto a = MkC [a] deriving Show

vacioC :: Conjunto a
vacioC = MkC []

agregarC :: Eq a => a -> Conjunto a -> Conjunto a
agregarC x (MkC xs) = 
    if elem x xs
        then MkC xs 
        else MkC (x : xs)

perteneceC :: Eq a => a -> Conjunto a -> Bool
perteneceC x (MkC xs) = elem x xs

cantidadC :: Eq a => Conjunto a -> Int
cantidadC (MkC xs) = length xs

borrarC :: Eq a => a -> Conjunto a -> Conjunto a
borrarC x (MkC xs) =
    if elem x xs
        then MkC (borrar x xs)
        else MkC xs

-- Función auxiliar privada
--
borrar :: Eq a => a -> [a] -> [a]
borrar _ [] = []
borrar e (x : xs) =
    if e == x
        then xs
        else x : borrar e xs

unionC :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
unionC (MkC xs) (MkC ys) =
    MkC (agregarSinDuplicados xs ys)

-- Función auxiliar privada
--
agregarSinDuplicados :: Eq a => [a] -> [a] -> [a]
agregarSinDuplicados [] ys       = ys
agregarSinDuplicados (x : xs) ys =
    if elem x ys
        then agregarSinDuplicados xs ys
        else agregarSinDuplicados xs (x : ys)

listaC :: Eq a => Conjunto a -> [a]
listaC (MkC xs) = xs
